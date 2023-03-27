(** Copyright 2022-2023, Grigory Aseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
module Format = Caml.Format

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * (id * value) list
  | VFunRec of id * value
  | VFunAPattern of active_pattern * value
  | VAPatternRes of id * value option
  | VSome of value
  | VNone
  | VUnit
  | VNil
[@@deriving show { with_path = false }]

let constr_vstring s = VString s
let constr_vbool b = VBool b
let constr_vint i = VInt i
let constr_vlist vs = VList vs
let constr_vtuple vs = VTuple vs
let constr_vfun p e env_vars = VFun (p, e, env_vars)
let constr_vfun_rec name func_body = VFunRec (name, func_body)
let constr_vapat_fun choice body = VFunAPattern (choice, body)
let constr_vapat_res name value = VAPatternRes (name, value)
let constr_vsome v = VSome v
let constr_vnone = VNone
let constr_vunit = VUnit
let constr_vnil = VNil

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (id, value, String.comparator_witness) Map.t

type interpret_error =
  | Division_by_zero (**  occurs when dividing an integer by zero  *)
  | UnboundValue of string
      (**  occurs when a variable name is called, but there is no existing binding  *)
  | UnboundConstructor of string
      (**  occurs when the name of the active pattern is called which has not been declared  *)
  | FunctionCompare
      (**  occurs when comparing expressions in which there are functions  *)
  | MatchFailure (**  occurs if the case is not matched  *)
  | EmptyProgram
      (**  occurs when an empty program (an empty list of expressions) is input  *)
  | TypeError (**  occurs when there is a mismatch of types  *)
  | Unreachable (**  should not occur  *)

let pp_interpret_error fmt = function
  | Division_by_zero -> Format.fprintf fmt "Exception: Division_by_zero."
  | UnboundValue s -> Format.fprintf fmt "Error: Unbound value %s" s
  | UnboundConstructor s -> Format.fprintf fmt "Error: Unbound constructor %s" s
  | FunctionCompare ->
    Format.fprintf fmt "Exception: Invalid_argument \"compare: functional value\""
  | MatchFailure ->
    Format.fprintf fmt "Exception: this pattern-matching is not exhaustive."
  | EmptyProgram -> Format.fprintf fmt "Error: the program was not provided or was empty"
  | TypeError -> Format.fprintf fmt "Error: type mismatch, a different type was expected"
  | Unreachable ->
    Format.fprintf fmt "Error: Unreachable error... Something went wrong..."
;;

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    match Map.find map key with
    | Some value -> return value
    | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
    | _ -> fail (UnboundValue key)
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) : sig
  val eval_program : program -> (value, interpret_error) M.t
end = struct
  open M
  open Environment (M)

  let rec eval expr env =
    let interpret_list_of_expr l constr =
      let* eval_list = all (List.map l ~f:(fun expr -> eval expr env)) in
      return @@ constr eval_list
    in
    match expr with
    | EConst v ->
      (match v with
       | CBool b -> return @@ constr_vbool b
       | CInt i -> return @@ constr_vint i
       | CString s -> return @@ constr_vstring s
       | CNil -> return @@ constr_vnil
       | CUnit -> return @@ constr_vunit)
    | EBinOp (op, l, r) ->
      let* rigth_val = eval r env in
      (match op, rigth_val with
       | Div, VInt 0 -> fail Division_by_zero
       | Mod, VInt 0 -> fail Division_by_zero
       | And, VBool false -> return @@ constr_vbool false
       | Or, VBool true -> return @@ constr_vbool true
       | op, rigth_val ->
         let* left_val = eval l env in
         (match op, left_val, rigth_val with
          | Add, VInt l, VInt r -> return @@ constr_vint (l + r)
          | Sub, VInt l, VInt r -> return @@ constr_vint (l - r)
          | Mul, VInt l, VInt r -> return @@ constr_vint (l * r)
          | Div, VInt l, VInt r -> return @@ constr_vint (l / r)
          | Mod, VInt l, VInt r -> return @@ constr_vint (l % r)
          | And, VBool b, VBool _ | Or, VBool b, VBool _ -> return @@ constr_vbool b
          | Cons, h, VList tl -> return @@ constr_vlist (h :: tl)
          | Cons, h, VNil -> return @@ constr_vlist (h :: [])
          | cmp_op, l, r ->
            let compute_cmp_op = function
              | Less -> return Poly.( < )
              | Leq -> return Poly.( <= )
              | Gre -> return Poly.( > )
              | Geq -> return Poly.( >= )
              | Eq -> return Poly.( = )
              | Neq -> return Poly.( <> )
              | _ -> fail Unreachable
            in
            let rec is_fun_in_vlist = function
              | [] -> false
              | VFun _ :: _ -> true
              | VTuple l :: tl | VList l :: tl -> is_fun_in_vlist l || is_fun_in_vlist tl
              | VSome v :: tl -> is_fun_in_vlist [ v ] || is_fun_in_vlist tl
              | _ :: tl -> is_fun_in_vlist tl
            in
            let rec eval_cmp op = function
              | VBool bl, VBool br ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp bl br)
              | VInt il, VInt ir ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp il ir)
              | VString sl, VString sr ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp sl sr)
              | VList ll, VList lr ->
                if is_fun_in_vlist ll || is_fun_in_vlist lr
                then fail FunctionCompare
                else
                  let* cmp = compute_cmp_op op in
                  return @@ constr_vbool (cmp ll lr)
              | VTuple tl, VTuple tr ->
                if is_fun_in_vlist tl || is_fun_in_vlist tr
                then fail FunctionCompare
                else
                  let* cmp = compute_cmp_op op in
                  return @@ constr_vbool (cmp tl tr)
              | VNil, VNil ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp [] [])
              | VUnit, VUnit ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp () ())
              | VSome x, VSome y ->
                if is_fun_in_vlist [ x; y ]
                then fail FunctionCompare
                else eval_cmp op (x, y)
              | VNone, VNone | VNone, VSome _ | VSome _, VNone ->
                let* cmp = compute_cmp_op op in
                return @@ constr_vbool (cmp l r)
              | _ -> fail Unreachable
            in
            eval_cmp cmp_op (l, r)))
    | EVar id -> find_val env id
    | EList l -> interpret_list_of_expr l constr_vlist
    | ETuple t -> interpret_list_of_expr t constr_vtuple
    | EIf (cond, e_then, e_else) ->
      let* cond_branch = eval cond env in
      (match cond_branch with
       | VBool b ->
         let e = if b then e_then else e_else in
         eval e env
       | _ -> fail TypeError)
    | EFun (pat, expr) -> return @@ constr_vfun pat expr (Map.to_alist env)
    | EApp (func, arg) ->
      let* fun_to_apply = eval func env in
      (match fun_to_apply with
       | VFun (pat, expr, fun_env) ->
         let* arg_to_apply = eval arg env in
         let* res = bind_fun_params ~env (pat, arg_to_apply) in
         eval expr (add_binds (add_binds empty fun_env) res)
       | VFunRec (name, VFun (pat, expr, fun_env)) ->
         let* arg_to_apply = eval arg env in
         let* res = bind_fun_params ~env (pat, arg_to_apply) in
         eval
           expr
           (add_binds
              (add_bind
                 (add_binds empty fun_env)
                 name
                 (VFunRec (name, VFun (pat, expr, fun_env))))
              res)
       | _ -> fail TypeError)
    | EMatch (expr_match, cases) ->
      let* val_match = eval expr_match env in
      let rec eval_match = function
        | (pat, expr) :: cases ->
          run
            (bind_fun_params ~env (pat, val_match))
            ~ok:(fun binds -> eval expr (add_binds env binds))
            ~err:(fun _ -> eval_match cases)
        | [] -> fail MatchFailure
      in
      eval_match cases
    | EAPattern (constr_id, args) ->
      (match constr_id, args with
       | "Some", arg :: [] ->
         let* opt_val = eval arg env in
         return @@ constr_vsome opt_val
       | "None", [] -> return constr_vnone
       | _, [] -> return @@ constr_vapat_res constr_id None
       | _, arg :: [] ->
         let* arg_val = eval arg env in
         (match arg_val with
          | VAPatternRes _ -> fail TypeError
          | _ -> return @@ constr_vapat_res constr_id (Some arg_val))
       | _ -> fail TypeError)
    | ELet (is_rec, bind_name, body) ->
      if is_rec
      then (
        match bind_name with
        | FName name ->
          let* func_body = eval body env in
          return @@ constr_vfun_rec name func_body
        | _ -> fail Unreachable)
      else (
        match bind_name with
        | FName _ -> eval body env
        | FAPattern a_pat ->
          let* fun_pat = eval body env in
          return @@ constr_vapat_fun a_pat fun_pat)
    | ELetIn (e_let, e_in) ->
      (match e_let with
       | ELet (_, FName f, _) ->
         let* v_let = eval e_let env in
         eval e_in (add_bind env f v_let)
       | _ -> fail Unreachable)

  and bind_fun_params ?(env = empty) =
    let bind_pat_list patl argl =
      let binded_list =
        List.fold2
          patl
          argl
          ~f:(fun acc pat arg ->
            let* acc = acc in
            let* binding = bind_fun_params ~env (pat, arg) in
            return (acc @ binding))
          ~init:(return [])
      in
      match binded_list with
      | Ok v -> v
      | _ -> fail MatchFailure
    in
    function
    | _, VAPatternRes _ -> fail MatchFailure
    | PWild, _ -> return []
    | PConst c, app_arg ->
      (match c, app_arg with
       | CBool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | CInt i1, VInt i2 when i1 = i2 -> return []
       | CString s1, VString s2 when String.equal s1 s2 -> return []
       | CNil, VNil | CUnit, VUnit -> return []
       | _ -> fail MatchFailure)
    | PVar var, app_arg -> return [ var, app_arg ]
    | PCons (p1, p2), VList vl ->
      (match p2, p1 with
       | PConst CNil, p1 ->
         (match p1 with
          | PConst CNil when List.is_empty vl -> return []
          | PWild | PVar _ | PList _ -> bind_fun_params ~env (p1, VList vl)
          | _ -> fail MatchFailure)
       | PCons (_, _), p1 ->
         (match p1 with
          | PCons (_, _) -> fail Unreachable
          | _ ->
            (match vl with
             | h :: tl ->
               let* head_bind = bind_fun_params ~env (p1, h) in
               let* tail_bind = bind_fun_params ~env (p2, VList tl) in
               return (head_bind @ tail_bind)
             | _ -> fail MatchFailure))
       | _ -> fail Unreachable)
    | PTuple pt, VTuple vt -> bind_pat_list pt vt
    | PList pl, VList vl -> bind_pat_list pl vl
    | PACase (acase_id, acase_args), value_to_match ->
      (match acase_id, acase_args, value_to_match with
       | "Some", acase_arg :: [], VSome v -> bind_fun_params ~env (acase_arg, v)
       | "None", [], VNone -> return []
       | _, _, VAPatternRes _ -> fail MatchFailure
       | "None", [], _ | "Some", _ :: [], _ -> fail MatchFailure
       | _ ->
         let* apat = find_val env acase_id in
         (match apat with
          | VFunAPattern (choice, VFun (apat_arg, apat_expr, apat_env)) ->
            (match choice with
             | SingleChoice (is_opt, _) ->
               let rec apply_fun_apat args func =
                 let rec convert_from_pat_to_val =
                   let convert_list_of_pat l constr =
                     let* eval_list =
                       all (List.map l ~f:(fun pat -> convert_from_pat_to_val pat))
                     in
                     return @@ constr eval_list
                   in
                   function
                   | PWild -> fail MatchFailure
                   | PACase (id, pats) ->
                     (match id, pats with
                      | "None", [] -> return constr_vnone
                      | "Some", arg :: [] ->
                        let* convert_arg = convert_from_pat_to_val arg in
                        (match convert_arg with
                         | VNone -> return constr_vnone
                         | _ -> return @@ constr_vsome convert_arg)
                      | _ -> fail MatchFailure)
                   | PConst c -> eval (EConst c) env
                   | PVar id -> eval (EVar id) env
                   | PTuple tpat -> convert_list_of_pat tpat constr_vtuple
                   | PList lpat -> convert_list_of_pat lpat constr_vtuple
                   | PCons (pat1, pat2) ->
                     let* lcons = convert_from_pat_to_val pat1 in
                     (match pat2 with
                      | PCons _ ->
                        let* rcons = convert_from_pat_to_val pat2 in
                        (match rcons with
                         | VList l -> return @@ constr_vlist (lcons :: l)
                         | _ -> fail MatchFailure)
                      | PConst CNil ->
                        (match lcons with
                         | VNil -> return @@ constr_vlist []
                         | VList _ -> return lcons
                         | _ -> fail MatchFailure)
                      | _ -> fail Unreachable)
                 in
                 let bind_result_apat res =
                   match func with
                   | VFun (apat_arg, apat_expr, apat_env) ->
                     let* bind_val_to_match =
                       bind_fun_params (apat_arg, value_to_match)
                     in
                     let* res_apat =
                       eval
                         apat_expr
                         (add_binds (add_binds empty apat_env) bind_val_to_match)
                     in
                     (match res_apat, is_opt, res with
                      | VAPatternRes (aconstr_id, None), false, PConst CUnit
                      | VSome (VAPatternRes (aconstr_id, None)), true, PConst CUnit
                        when String.equal aconstr_id acase_id -> return []
                      | VAPatternRes (aconstr_id, Some v), false, _
                      | VSome (VAPatternRes (aconstr_id, Some v)), true, _
                        when String.equal aconstr_id acase_id -> bind_fun_params (res, v)
                      | _, false, _ -> bind_fun_params (res, res_apat)
                      | VSome v, true, _ -> bind_fun_params (res, v)
                      | _ -> fail MatchFailure)
                   | _ -> fail MatchFailure
                 in
                 let apply_arg arg other_args =
                   match func with
                   | VFun (apat_arg, apat_expr, apat_env) ->
                     let* convert_arg = convert_from_pat_to_val arg in
                     let* bind_arg = bind_fun_params (apat_arg, convert_arg) in
                     let* eval_new_func_apat =
                       eval apat_expr (add_binds (add_binds empty apat_env) bind_arg)
                     in
                     apply_fun_apat other_args eval_new_func_apat
                   | _ -> fail MatchFailure
                 in
                 match args with
                 | [] -> bind_result_apat (PConst CUnit)
                 | h :: [] ->
                   run
                     (apply_arg h [])
                     ~ok:(fun res -> return res)
                     ~err:(fun _ -> bind_result_apat h)
                 | h :: tl -> apply_arg h tl
               in
               apply_fun_apat acase_args (VFun (apat_arg, apat_expr, apat_env))
             | MultipleChoice _ ->
               (match acase_args with
                | res when List.length res < 2 ->
                  let* bind_matching_val =
                    bind_fun_params ~env (apat_arg, value_to_match)
                  in
                  let* eval_res_apat =
                    eval
                      apat_expr
                      (add_binds (add_binds empty apat_env) bind_matching_val)
                  in
                  (match eval_res_apat with
                   | VAPatternRes (apat_id, opt_res) when String.equal apat_id acase_id ->
                     (match opt_res, res with
                      | Some v, res :: [] -> bind_fun_params (res, v)
                      | None, res :: [] -> bind_fun_params (res, VUnit)
                      | None, _ -> return []
                      | _ -> fail MatchFailure)
                   | _ -> fail MatchFailure)
                | _ -> fail Unreachable))
          | _ -> fail Unreachable))
    | _ -> fail MatchFailure
  ;;

  let eval_program (program : expr list) : (value, interpret_error) t =
    let rec helper env = function
      | h :: [] -> eval h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval h env in
        let eval_env =
          let rec eval_env_apat env = function
            | [] -> env
            | h :: tl -> add_bind (eval_env_apat env tl) h eval_h
          in
          match h with
          | (ELet (_, FName f, _) | ELet (_, FAPattern (SingleChoice (_, f)), _))
            when not @@ String.equal f "_" -> add_bind env f eval_h
          | ELet (_, FAPattern (MultipleChoice fl), _) -> eval_env_apat env fl
          | _ -> env
        in
        helper eval_env tl
    in
    helper empty program
  ;;
end

module InterpretResult = Interpret (struct
  include Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) monad f = bind monad ~f
end)

let eval_program = InterpretResult.eval_program

let run input =
  match Parser.parse input with
  | Ok ast ->
    (match eval_program ast with
     | Ok res -> pp_value Format.std_formatter res
     | Error e -> Format.printf "(Error while interpreting): %a" pp_interpret_error e)
  | Error e -> Format.printf "(Error while parsing): %s" e
;;

(** Tests for interpretator *)

(**  1 + 2 + 3 + 4 * 5 + 6 / 3 + (10 - 5) / 5  *)
let test =
  [ EBinOp
      ( Add
      , EBinOp
          ( Add
          , EBinOp
              ( Add
              , EBinOp
                  (Add, EBinOp (Add, EConst (CInt 1), EConst (CInt 2)), EConst (CInt 3))
              , EBinOp (Mul, EConst (CInt 4), EConst (CInt 5)) )
          , EBinOp (Div, EConst (CInt 6), EConst (CInt 3)) )
      , EBinOp (Div, EBinOp (Sub, EConst (CInt 10), EConst (CInt 5)), EConst (CInt 5)) )
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 29) -> true
  | _ -> false
;;

(**  [let sum x y = x + y; sum (sum 700 70) 7]  *)
let test =
  [ ELet
      ( false
      , FName "sum"
      , EFun (PVar "x", EFun (PVar "y", EBinOp (Add, EVar "x", EVar "y"))) )
  ; EApp
      ( EApp (EVar "sum", EApp (EApp (EVar "sum", EConst (CInt 700)), EConst (CInt 70)))
      , EConst (CInt 7) )
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 777) -> true
  | _ -> false
;;

(**  fibonacci 20  *)

let test =
  [ ELet
      ( true
      , FName "fibonacci"
      , EFun
          ( PVar "n"
          , EIf
              ( EBinOp (Leq, EVar "n", EConst (CInt 1))
              , EConst (CInt 1)
              , EBinOp
                  ( Add
                  , EApp (EVar "fibonacci", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                  , EApp (EVar "fibonacci", EBinOp (Sub, EVar "n", EConst (CInt 2))) ) )
          ) )
  ; EApp (EVar "fibonacci", EConst (CInt 20))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 10946) -> true
  | _ -> false
;;

(**  factorial 20  *)

let test =
  [ ELet
      ( true
      , FName "factorial"
      , EFun
          ( PVar "n"
          , EIf
              ( EBinOp (Leq, EVar "n", EConst (CInt 0))
              , EConst (CInt 1)
              , EMatch
                  ( EVar "n"
                  , [ PConst (CInt 1), EConst (CInt 1)
                    ; ( PWild
                      , EBinOp
                          ( Mul
                          , EApp
                              (EVar "factorial", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                          , EVar "n" ) )
                    ] ) ) ) )
  ; EApp (EVar "factorial", EConst (CInt 20))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 2432902008176640000) -> true
  | _ -> false
;;

(**  Active patterns tests  *)

(**  let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd  *)
let test_for_multy_pat =
  [ ELet
      ( false
      , FAPattern (MultipleChoice [ "Even"; "Odd" ])
      , EFun
          ( PVar "input"
          , EIf
              ( EBinOp (Eq, EBinOp (Mod, EVar "input", EConst (CInt 2)), EConst (CInt 0))
              , EAPattern ("Even", [])
              , EAPattern ("Odd", []) ) ) )
  ; ELet
      ( false
      , FName "recognize"
      , EFun
          ( PVar "input"
          , EMatch
              ( EVar "input"
              , [ PACase ("Even", []), ETuple [ EVar "input"; EConst (CString "even") ]
                ; PACase ("Odd", []), ETuple [ EVar "input"; EConst (CString "odd") ]
                ] ) ) )
  ]
;;

let test = test_for_multy_pat @ [ EApp (EVar "recognize", EConst (CInt 20)) ]

let%test _ =
  match eval_program test with
  | Ok (VTuple [ VInt 20; VString "even" ]) -> true
  | _ -> false
;;

let test = test_for_multy_pat @ [ EApp (EVar "recognize", EConst (CInt 21)) ]

let%test _ =
  match eval_program test with
  | Ok (VTuple [ VInt 21; VString "odd" ]) -> true
  | _ -> false
;;

(**  let (|Default|) onNone value =
    match value with
    | None -> onNone
    | Some e -> e  *)
let test_apat_with_args_in_func_arg =
  [ ELet
      ( false
      , FAPattern (SingleChoice (false, "Default"))
      , EFun
          ( PVar "onNone"
          , EFun
              ( PVar "value"
              , EMatch
                  ( EVar "value"
                  , [ PACase ("None", []), EVar "onNone"
                    ; PACase ("Some", [ PVar "e" ]), EVar "e"
                    ] ) ) ) )
  ; ELet
      ( false
      , FName "greet"
      , EFun
          ( PACase ("Default", [ PConst (CString "random citizen"); PVar "name" ])
          , EVar "name" ) )
  ]
;;

let test =
  test_apat_with_args_in_func_arg @ [ EApp (EVar "greet", EAPattern ("None", [])) ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "random citizen") -> true
  | _ -> false
;;

let test =
  test_apat_with_args_in_func_arg
  @ [ EApp (EVar "greet", EAPattern ("Some", [ EConst (CString "George") ])) ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "George") -> true
  | _ -> false
;;

(**  let (| Foo|_|) s x = if x = s then Some Foo else None  *)
let test_apat_with_args_opt =
  [ ELet
      ( false
      , FAPattern (SingleChoice (true, "Foo"))
      , EFun
          ( PVar "x"
          , EFun
              ( PVar "y"
              , EIf
                  ( EBinOp (Eq, EVar "x", EVar "y")
                  , EAPattern ("Some", [ EAPattern ("Foo", []) ])
                  , EAPattern ("None", []) ) ) ) )
  ; ELet
      ( false
      , FName "is_equal"
      , EFun
          ( PVar "x"
          , EFun
              ( PVar "y"
              , EMatch
                  ( EVar "x"
                  , [ PACase ("Foo", [ PVar "y" ]), EConst (CString "x = y")
                    ; PWild, EConst (CString "x <> y")
                    ] ) ) ) )
  ]
;;

let test =
  test_apat_with_args_opt
  @ [ EApp (EApp (EVar "is_equal", EConst (CInt 2)), EConst (CInt 2)) ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "x = y") -> true
  | _ -> false
;;

let test =
  test_apat_with_args_opt
  @ [ EApp (EApp (EVar "is_equal", EConst (CInt 3)), EConst (CInt 2)) ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "x <> y") -> true
  | _ -> false
;;

let test =
  test_apat_with_args_opt
  @ [ EApp
        ( EApp (EVar "is_equal", EAPattern ("Some", [ EConst (CInt 1) ]))
        , EAPattern ("None", []) )
    ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "x <> y") -> true
  | _ -> false
;;

let test =
  test_apat_with_args_opt
  @ [ EApp
        ( EApp (EVar "is_equal", EAPattern ("Some", [ EConst (CString "Good luck") ]))
        , EAPattern ("Some", [ EConst (CString "Good luck") ]) )
    ]
;;

let%test _ =
  match eval_program test with
  | Ok (VString "x = y") -> true
  | _ -> false
;;

(**  Error tests  *)
let test = [ EBinOp (Div, EConst (CInt 777), EConst (CInt 0)) ]

let%test _ =
  match eval_program test with
  | Error Division_by_zero -> true
  | _ -> false
;;

let test = [ EBinOp (Add, EVar "x", EConst (CInt 1)) ]

let%test _ =
  match eval_program test with
  | Error (UnboundValue "x") -> true
  | _ -> false
;;

let test =
  [ EBinOp
      ( Gre
      , EAPattern
          ("Some", [ EFun (PVar "x", EFun (PVar "y", EBinOp (Add, EVar "x", EVar "y"))) ])
      , EAPattern
          ("Some", [ EFun (PVar "x", EFun (PVar "y", EBinOp (Sub, EVar "x", EVar "y"))) ])
      )
  ]
;;

let%test _ =
  match eval_program test with
  | Error FunctionCompare -> true
  | _ -> false
;;

let test = []

let%test _ =
  match eval_program test with
  | Error EmptyProgram -> true
  | _ -> false
;;
