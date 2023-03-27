(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Inferencer
open Typing

module EnvrMap = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

exception Not_bound

let lookup id env =
  try EnvrMap.find id env with
  | Not_found -> raise Not_bound
;;

let empty_env = EnvrMap.empty

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type exval =
  | TupleVal of exval list
  | ListVal of exval list
  | SomeVal of exval
  | NoneVal
  | FunVal of pt * exp * env Lazy.t * scan_info
  | IntVal of int
  | StringVal of string
  | BoolVal of bool
[@@deriving show { with_path = false }]

and error =
  | Match_fail of pt * exval
  | Match_exhaust of exp
  | Wrong_arg_pat of pt
  | Wrong_arg of exp
  | Wrong_un_op of un_op * exval
  | Wrong_bin_op of bin_op * exval * exval
  | Not_bool_condition of exp
  | Not_bound of id
  | Not_function of exp
  | Let_rec_only_vars of pt
  | Division_by_zero
[@@deriving show { with_path = false }]

and env = exval EnvrMap.t [@@deriving show { with_path = false }]
and scan_info = Info of env * id list

module Interpret (M : MONAD_FAIL) = struct
  open M

  let pp_value (k, v) =
    let open Format in
    let rec helper _ = function
      | NoneVal -> sprintf "NONE"
      | SomeVal x -> sprintf "SOME %s" @@ helper std_formatter x
      | BoolVal b -> sprintf "%b" b
      | FunVal (_, _, _, _) -> sprintf "fn"
      | IntVal n -> sprintf (if n < 0 then "~%d" else "%d") (Int.abs n)
      | TupleVal l ->
        let rec elems = function
          | [ hd ] -> helper std_formatter hd
          | hd :: tail -> sprintf "%s, %s" (helper std_formatter hd) (elems tail)
          | _ -> ""
        in
        sprintf "(%s)" (elems l)
      | ListVal l ->
        let rec elems = function
          | [ hd ] -> helper std_formatter hd
          | hd :: tail -> sprintf "%s, %s" (helper std_formatter hd) (elems tail)
          | _ -> ""
        in
        sprintf "[%s]" (elems l)
      | StringVal s -> sprintf "%S" s
    in
    sprintf "val %s = %s: %!" k (helper std_formatter v)
  ;;

  let to_string = function
    | TupleVal _ -> "tuple"
    | ListVal _ -> "list"
    | SomeVal _ | NoneVal -> "option"
    | FunVal _ -> "fn"
    | IntVal _ -> "int"
    | StringVal _ -> "string"
    | BoolVal _ -> "bool"
  ;;

  let pp_error e =
    let open Format in
    let helper _ = function
      | Match_fail (PtTuple _, TupleVal _) ->
        printf
          "Elaboration failed: Expected and given record types has different number of \
           entries"
      | Match_fail (_, b) ->
        printf "Elaboration failed: Didn't expected type %s" (to_string b)
      | Match_exhaust exp ->
        print_string "WARN: Pattern matching is not exhaustive: ";
        pp_exp std_formatter exp
      | Wrong_arg_pat x ->
        printf "Elaboration failed: Wrong fn argument: ";
        pp_pt std_formatter x
      | Wrong_arg x ->
        printf "Elaboration failed: Wrong fun application argument: ";
        pp_exp std_formatter x
      | Wrong_un_op (un, x) ->
        printf
          "Elaboration failed: Type clash. Unary %s operator cannot take an argument of \
           type %s"
          (show_un_op un)
          (to_string x)
      | Wrong_bin_op (bn, ex1, ex2) ->
        printf
          "Elaboration failed: Type clash. Binary %s operator cannot take arguments of \
           type %s and %s"
          (show_bin_op bn)
          (to_string ex1)
          (to_string ex2)
      | Not_bool_condition _ ->
        printf "Elaboration failed: Type clash. If condition must be of type bool"
      | Not_bound id -> printf "Elaboration failed: Unbound value identifier %s" id
      | Not_function exp ->
        printf "Elaboration failed: %s is not a function" (show_exp exp)
      | Let_rec_only_vars _ -> printf {|Parsing failed: Using "rec" requires a variable|}
      | Division_by_zero -> printf "SML exception: divising by zero"
    in
    helper std_formatter e;
    printf "\n"
  ;;

  let exd_env id v env = EnvrMap.add id v env

  let extend_state env binds =
    List.fold_left (fun env (id, v) -> exd_env id v env) env binds
  ;;

  let lookup_env id env =
    try return (lookup id env) with
    | Not_bound -> fail (Not_bound id)
  ;;

  let fold list ~f ~init =
    let rec helper acc = function
      | hd :: tl -> helper (acc >>= fun acc -> f acc hd) tl
      | [] -> acc
    in
    helper (return init) list
  ;;

  let rec pt_match pt var =
    match pt, var with
    | PtNone, NoneVal -> return []
    | PtNil, ListVal [] -> return []
    | PtWild, _ -> return []
    | PtSome x, SomeVal y -> pt_match x y
    | PtVar id, v -> return [ id, v, pp_value (id, v) ]
    | PtTuple pats, TupleVal vars ->
      let pats', vars' = pats, vars in
      (match pats, vars with
       | hd_p :: tl_p, hd_v :: tl_v ->
         let* hd_bind = pt_match hd_p hd_v in
         let* tl_bind = pt_match (PtTuple tl_p) (TupleVal tl_v) in
         return (hd_bind @ tl_bind)
       | [], [] -> return []
       | _ -> fail (Match_fail (PtTuple pats', TupleVal vars')))
    | PtCons (pat1, pat2), ListVal (hd :: tl) ->
      let* hd_matched = pt_match pat1 hd in
      let* tl_matched = pt_match pat2 (ListVal tl) in
      return (hd_matched @ tl_matched)
    | PtConst x, v ->
      (match x, v with
       | CInt a, IntVal b when a = b -> return []
       | CString a, StringVal b when a = b -> return []
       | CBool a, BoolVal b when a = b -> return []
       | _ -> fail (Match_fail (PtConst x, v)))
    | a, b -> fail (Match_fail (a, b))
  ;;

  let rec apply_bin_op op x y =
    match op, x, y with
    | Eq, IntVal x, IntVal y -> return (BoolVal (x == y))
    | Eq, StringVal x, StringVal y -> return (BoolVal (x == y))
    | Eq, BoolVal x, BoolVal y -> return (BoolVal (x == y))
    | Eq, TupleVal x, TupleVal y -> return (BoolVal (x == y))
    | Eq, ListVal x, ListVal y -> return (BoolVal (x == y))
    | Eq, NoneVal, NoneVal -> return (BoolVal true)
    | Neq, IntVal x, IntVal y -> return (BoolVal (x != y))
    | Neq, StringVal x, StringVal y -> return (BoolVal (x != y))
    | Neq, BoolVal x, BoolVal y -> return (BoolVal (x != y))
    | Neq, TupleVal x, TupleVal y -> return (BoolVal (x != y))
    | Neq, ListVal x, ListVal y -> return (BoolVal (x != y))
    | And, BoolVal x, BoolVal y -> return (BoolVal (x && y))
    | Or, BoolVal x, BoolVal y -> return (BoolVal (x || y))
    | Gre, IntVal x, IntVal y -> return (BoolVal (x > y))
    | Gre, StringVal x, StringVal y -> return (BoolVal (x > y))
    | Gre, TupleVal x, TupleVal y -> return (BoolVal (x > y))
    | Gre, ListVal x, ListVal y -> return (BoolVal (x > y))
    | Geq, IntVal x, IntVal y -> return (BoolVal (x >= y))
    | Geq, StringVal x, StringVal y -> return (BoolVal (x >= y))
    | Geq, TupleVal x, TupleVal y -> return (BoolVal (x >= y))
    | Geq, ListVal x, ListVal y -> return (BoolVal (x >= y))
    | Less, IntVal x, IntVal y -> return (BoolVal (x < y))
    | Less, StringVal x, StringVal y -> return (BoolVal (x < y))
    | Less, TupleVal x, TupleVal y -> return (BoolVal (x < y))
    | Less, ListVal x, ListVal y -> return (BoolVal (x < y))
    | Leq, IntVal x, IntVal y -> return (BoolVal (x <= y))
    | Leq, StringVal x, StringVal y -> return (BoolVal (x <= y))
    | Leq, TupleVal x, TupleVal y -> return (BoolVal (x <= y))
    | Leq, ListVal x, ListVal y -> return (BoolVal (x <= y))
    | Mul, IntVal x, IntVal y -> return (IntVal (x * y))
    | Div, IntVal _, IntVal y when y = 0 -> fail Division_by_zero
    | Div, IntVal x, IntVal y -> return (IntVal (x / y))
    | Sub, IntVal x, IntVal y -> return (IntVal (x - y))
    | Add, IntVal x, IntVal y -> return (IntVal (x + y))
    | Eq, SomeVal x, SomeVal y -> apply_bin_op Eq x y
    | a, b, c -> fail (Wrong_bin_op (a, b, c))
  ;;

  let apply_un_op op x =
    match op, x with
    | Not, BoolVal x -> return (BoolVal (not x))
    | Minus, IntVal x -> return (IntVal (-x))
    | a, b -> fail (Wrong_un_op (a, b))
  ;;

  let rec efun env env_b keys_b = function
    | EFun (pt, exp) ->
      (match pt with
       | PtVar id -> efun env env_b (id :: keys_b) exp
       | PtWild -> efun env env_b ("" :: keys_b) exp
       | p -> fail (Wrong_arg_pat p))
    | _ -> return (Info (env_b, List.rev keys_b))

  and scan_app env = function
    | EApp (exp_h1, exp_h2) ->
      let* Info (basic, keys), fstate, body = scan_app env exp_h1 in
      (match keys with
       | hd :: tl ->
         let* evaled = eval_expr env exp_h2 in
         let new_state = exd_env hd evaled fstate in
         let new_basic = exd_env hd evaled basic in
         return (Info (new_basic, tl), new_state, body)
       | [] -> fail (Wrong_arg exp_h2))
    | EVar name ->
      let* evaled = eval_expr env (EVar name) in
      (match evaled with
       | FunVal (_, body, fstate, Info (basic, keys)) ->
         let fstate = Lazy.force fstate in
         return (Info (basic, keys), fstate, body)
       | _ -> fail (Not_function (EVar name)))
    | e -> fail (Not_function e)

  and eval_expr env = function
    | ENil -> return (ListVal [])
    | EVar x -> run (lookup_env x env) ~ok:return ~err:fail
    | ENone -> return NoneVal
    | EIf (exp1, exp2, exp3) ->
      run
        (eval_expr env exp1)
        ~ok:
          (function
           | BoolVal false -> eval_expr env exp3
           | BoolVal true -> eval_expr env exp2
           | _ -> fail (Not_bool_condition exp1))
        ~err:fail
    | EMatch (exp, mathchings) ->
      let* evaled = eval_expr env exp in
      let rec do_match = function
        | (pt, exp) :: tl ->
          run
            (pt_match pt evaled)
            ~ok:(fun binds ->
              let env = extend_state env (List.map (fun (a, b, _) -> a, b) binds) in
              eval_expr env exp)
            ~err:(fun _ -> do_match tl)
        | [] -> fail (Match_exhaust (EMatch (exp, mathchings)))
      in
      do_match mathchings
    | EConst x ->
      (match x with
       | CInt x -> return (IntVal x)
       | CBool x -> return (BoolVal x)
       | CString x -> return (StringVal x))
    | EBinOp (op, x, y) ->
      let* x_exp = eval_expr env x in
      let* y_exp = eval_expr env y in
      run (apply_bin_op op x_exp y_exp) ~ok:return ~err:fail
    | EUnOp (op, x) ->
      let* x_exp = eval_expr env x in
      run (apply_un_op op x_exp) ~ok:return ~err:fail
    | ESome x ->
      let* evaled = eval_expr env x in
      return (SomeVal evaled)
    | ELet (binds, exp1) ->
      let gen_env =
        fold binds ~init:env ~f:(fun env binding ->
          let* _, st = eval_bind env binding in
          return st)
      in
      run gen_env ~ok:(fun s -> eval_expr s exp1) ~err:fail
    | ETuple exps ->
      M.all (List.map (eval_expr env) exps) >>= fun x -> return (TupleVal x)
    | EApp (exp1, exp2) ->
      let* Info (basic, keys), new_state, body = scan_app env (EApp (exp1, exp2)) in
      let new_state = EnvrMap.union (fun _ _ y -> Some y) empty_env new_state in
      let rec helper = function
        | EFun (pt, exp) ->
          (match pt with
           | PtVar name ->
             run
               (lookup_env name basic)
               ~ok:(fun _ -> helper exp)
               ~err:(fun _ -> return (EFun (pt, exp)))
           | PtWild -> helper exp
           | p -> fail (Wrong_arg_pat p))
        | e -> return e
      in
      let* body = helper body in
      (match body with
       | EFun (pt, exp) -> return @@ FunVal (pt, exp, lazy new_state, Info (basic, keys))
       | _ -> eval_expr new_state body)
    | ECons (exp1, exp2) ->
      let* evaled_exp1 = eval_expr env exp1 in
      let* evaled_exp2 = eval_expr env exp2 in
      (match evaled_exp2 with
       | ListVal list -> return (ListVal (evaled_exp1 :: list))
       | x -> return (ListVal [ evaled_exp1; x ]))
    | EFun (pt, exp) ->
      let* scan_info = efun env empty_env [] (EFun (pt, exp)) in
      return (FunVal (pt, exp, lazy env, scan_info))

  and eval_bind env (is_rec, pt, exp) =
    if not is_rec
    then
      let* evd = eval_expr env exp in
      let* bs = pt_match pt evd in
      let exd = extend_state env (List.map (fun (a, b, _) -> a, b) bs) in
      return (bs, exd)
    else
      let* id =
        match pt with
        | PtVar id -> return id
        | other -> fail (Let_rec_only_vars other)
      in
      let* tmp =
        match exp with
        | EFun (pt, body) ->
          let* scan_info = efun env empty_env [] (EFun (pt, body)) in
          let rec new_env =
            lazy (exd_env id (FunVal (pt, body, new_env, scan_info)) env)
          in
          return (FunVal (pt, body, new_env, scan_info))
        | other -> eval_expr env other
      in
      let exded = exd_env id tmp env in
      let* evd = eval_expr exded exp in
      return ([ id, evd, pp_value (id, evd) ], exd_env id evd exded)
  ;;

  let eval_dec env = function
    | DLet bind -> eval_bind env bind
  ;;

  let eval_prog prog =
    let* binds, typs, _, _ =
      fold
        ~f:(fun (binds, typs, env, environment) dec ->
          let tcheck_result =
            match dec with
            | DLet (_, PtWild, _) -> []
            | DLet (_, PtVar _, _) -> [ run_inference dec environment ]
            | DLet (_, PtTuple a, ETuple b) when List.length a = List.length b ->
              List.map
                (fun (x, y) -> run_inference (DLet (false, x, y)) environment)
                (List.combine a b)
            | _ -> []
          in
          let rec get_last_env = function
            | [ Ok (env, _) ] -> env
            | _ :: tl -> get_last_env tl
            | _ -> environment
          in
          let* new_binds, new_state = eval_dec env dec in
          let environment' = get_last_env tcheck_result in
          return (new_binds :: binds, tcheck_result :: typs, new_state, environment'))
        ~init:([], [], empty_env, TypeEnv.empty)
        prog
    in
    return (binds |> List.rev |> List.flatten, typs |> List.rev |> List.flatten)
  ;;
end

module InterpreterResult = struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) x f = x >>= f
end

let eval_pp _ code =
  let open Format in
  let open Interpret (InterpreterResult) in
  match Parser.parse Parser.prog code with
  | Ok prog ->
    InterpreterResult.run
      (eval_prog prog)
      ~err:(fun x -> pp_error x)
      ~ok:(fun (x, y) ->
        let error_check =
          List.find_opt
            (function
             | Error _ -> true
             | _ -> false)
            y
        in
        match error_check with
        | None ->
          List.iter
            (fun ((_, _, a), b) ->
              match b with
              | Ok (_, typ) ->
                print_string a;
                print_typ typ
              | Error e -> print_type_error e)
            (List.combine x y)
        | Some (Error e) -> print_type_error e
        | _ -> print_type_error `Unreachable)
  | _ -> Printf.printf "Parse error\n"
;;
