(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Typing

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Int.comparator_witness) Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun monad f state ->
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok x -> f x last
 ;;

  let fail err state = state, Result.fail err
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f state ->
    match x state with
    | state, Ok x -> state, Ok (f x)
    | state, Error e -> state, Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> List.exists typ_list ~f:(occurs_in v)
    | TList typ | TOption typ -> occurs_in v typ
    | _ -> false
  ;;

  let unpack_vars =
    let empty = Set.empty (module Int) in
    let rec ehelper acc = function
      | TVar (n, _) -> Set.add acc n
      | TArr (left, right) -> ehelper (ehelper acc left) right
      | TTuple typ_list ->
        List.fold_right typ_list ~f:(fun t s -> Set.union s (ehelper empty t)) ~init:acc
      | TList typ | TOption typ -> ehelper acc typ
      | TGround _ | TEqualityVar _ -> acc
    in
    ehelper empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> typ -> t R.t
  val find_exn : fresh -> t -> typ
  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val add : t -> fresh -> typ -> t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  type t = (fresh, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping key value =
    if Type.occurs_in key value then fail `OccursCheck else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Base.Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Map.find_exn subst key
  let find key subst = Map.find subst key
  let remove subst key = Map.remove subst key
  let add subst key value = Map.update subst key ~f:(fun _ -> value)

  let apply s =
    let rec ehelper = function
      | TVar (n, b) ->
        (match find_exn n s with
         | exception Not_found_s _ -> TVar (n, b)
         | x -> x)
      | TEqualityVar (n, b) ->
        (match find_exn n s with
         | exception Not_found_s _ -> TEqualityVar (n, b)
         | x -> x)
      | TArr (left, right) -> arrow_t (ehelper left) (ehelper right)
      | TTuple typ_list -> tuple_t @@ List.map typ_list ~f:ehelper
      | TList typ -> list_t @@ ehelper typ
      | TOption typ -> option_t @@ ehelper typ
      | ground -> ground
    in
    ehelper
  ;;

  let rec unify l r =
    match l, r with
    | TGround l, TGround r when l == r -> return empty
    | TGround _, TGround _ | TEqualityVar _, TArr _ | TArr _, TEqualityVar _ ->
      fail @@ `UnificationFailed (l, r)
    | (TVar (a, _), TVar (b, _) | TEqualityVar (a, _), TEqualityVar (b, _)) when a = b ->
      return empty
    | TVar (b, _), t | t, TVar (b, _) | TEqualityVar (b, _), t | t, TEqualityVar (b, _) ->
      singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose_all [ subs1; subs2 ]
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match List.zip typ_list_l typ_list_r with
       | List.Or_unequal_lengths.Unequal_lengths -> fail @@ `UnificationFailed (l, r)
       | List.Or_unequal_lengths.Ok zipped_list ->
         List.fold_right
           zipped_list
           ~f:(fun (x, y) subst ->
             let* head_sub = unify x y in
             let* subst = subst in
             compose head_sub subst)
           ~init:(return empty))
    | TList typ1, TList typ2 | TOption typ1, TOption typ2 -> unify typ1 typ2
    | _ -> fail @@ `UnificationFailed (l, r)

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return @@ Base.Map.update acc k ~f:(fun _ -> v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module VarSet = struct
  let fold f ini set =
    Set.fold set ~init:ini ~f:(fun acc x ->
      let open R.Syntax in
      let* acc = acc in
      f acc x)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | s, t -> (not (Set.mem s v)) && Type.occurs_in v t
  ;;

  let unpack_vars = function
    | s, t -> Set.diff (Type.unpack_vars t) s
  ;;

  let apply sub (s, t) =
    let s2 = Set.fold s ~init:sub ~f:(fun acc k -> Subst.remove acc k) in
    s, Subst.apply s2 t
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, String.comparator_witness) Map.t

  let extend env id scheme = Map.update env id ~f:(fun _ -> scheme)
  let empty = Map.empty (module String)

  let unpack_vars : t -> (type_variable_number, Int.comparator_witness) Set.t =
    Map.fold
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.unpack_vars data))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| var_t
let fresh_eq_var = fresh >>| eqvar_t
let fresh_val = fresh >>| val_t

let rec is_restricted = function
  | TArr (l, r) -> is_restricted l || is_restricted r
  | TVar (_, b) | TEqualityVar (_, b) -> b
  | TTuple list ->
    (match list with
     | a :: b -> is_restricted a || is_restricted (TTuple b)
     | [] -> false)
  | TList t | TOption t -> is_restricted t
  | TGround _ -> false
;;

let instantiate : scheme -> typ R.t =
 fun (set, t) ->
  VarSet.fold
    (fun typ name ->
      let* f = if is_restricted typ then fresh_val else fresh_var in
      let* s = Subst.singleton name f in
      return (Subst.apply s typ))
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env typ ->
  let free = Set.diff (Type.unpack_vars typ) (TypeEnv.unpack_vars env) in
  free, typ
;;

let lookup_env e map =
  match Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans, map)
;;

let rec find_identifiers = function
  | PtTuple expr_list ->
    List.fold_right
      ~f:(fun expression acc -> Base.Set.union (find_identifiers expression) acc)
      ~init:(Set.empty (module String))
      expr_list
  | PtCons (head, tail) -> Base.Set.union (find_identifiers head) (find_identifiers tail)
  | PtVar id -> Base.Set.of_list (module String) [ id ]
  | PtSome x -> find_identifiers x
  | _ -> Set.empty (module String)
;;

let rec restrict = function
  | TVar (n, false) -> TVar (n, true)
  | TEqualityVar (n, false) -> TEqualityVar (n, true)
  | TArr (left, right) -> TArr (restrict left, restrict right)
  | TTuple lst -> TTuple (List.map lst ~f:restrict)
  | TList typ -> TList (restrict typ)
  | TOption typ -> TOption (restrict typ)
  | t -> t
;;

let check_restrict exp typ =
  match exp with
  | ELet _ | EMatch _ | EApp (_, _) | EIf _ -> restrict typ
  | _ -> typ
;;

let rec get_name = function
  | EApp (l, _) -> get_name l
  | EVar name -> return name
  | _ -> fail `Not_function
;;

let infer =
  let rec pthelper env = function
    | PtWild ->
      let* fresh_var = fresh_var in
      return (Subst.empty, fresh_var)
    | PtVar ident ->
      (match Map.find env ident with
       | None ->
         let* fresh_var = fresh_var in
         return (Subst.empty, fresh_var)
       | Some scheme ->
         let* ans = instantiate scheme in
         return (Subst.empty, ans))
    | PtConst literal ->
      (match literal with
       | CInt _ -> return (Subst.empty, int_typ)
       | CString _ -> return (Subst.empty, string_typ)
       | CBool _ -> return (Subst.empty, bool_typ))
    | PtCons (elem, list) ->
      let* elem_subst, elem_type = pthelper env elem in
      let* list_subst, list_type = pthelper env list in
      let* subst' = unify (list_t elem_type) list_type in
      let* final_subst = Subst.compose_all [ elem_subst; list_subst; subst' ] in
      return (final_subst, Subst.apply subst' list_type)
    | PtSome x ->
      let* content_subst, content_typ = pthelper env x in
      return (content_subst, option_t content_typ)
    | PtNone ->
      let* content_subst, content_typ =
        let* fresh_var = fresh_var in
        return (Subst.empty, fresh_var)
      in
      return (content_subst, option_t content_typ)
    | PtNil ->
      let* fresh_var = fresh_var in
      return (Subst.empty, TList fresh_var)
    | PtTuple list ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_type = pthelper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_type = subst_tuple subst' tail in
          return (final_subst, head_type :: tail_type)
      in
      let* final_subst, typ_list = subst_tuple Subst.empty list in
      return (final_subst, tuple_t @@ List.map typ_list ~f:(Subst.apply final_subst))
  in
  let rec ehelper : TypeEnv.t -> exp -> (Subst.t * typ * TypeEnv.t) R.t =
   fun env -> function
    | EConst x ->
      (match x with
       | CInt _ -> return (Subst.empty, int_typ, env)
       | CString _ -> return (Subst.empty, string_typ, env)
       | CBool _ -> return (Subst.empty, bool_typ, env))
    | EVar identifier -> lookup_env identifier env
    | ENil ->
      let* fresh_var = fresh_var in
      return (Subst.empty, TList fresh_var, env)
    | EUnOp (op, expr) ->
      let operand_type =
        match op with
        | Minus -> int_typ
        | Not -> bool_typ
      in
      let* subst, t, _ = ehelper env expr in
      let* subst' = unify t operand_type in
      let* final_subst = Subst.compose subst' subst in
      return (final_subst, operand_type, env)
    | EBinOp (op, left, right) ->
      let* left_subst, left_type, _ = ehelper env left in
      let* right_subst, right_type, _ = ehelper env right in
      (match op with
       | Add | Sub | Mul | Div ->
         let* subst' = unify left_type int_typ in
         let* subst'' = unify right_type int_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         return (final_subst, int_typ, env)
       | Gre | Geq | Less | Leq ->
         let* subst' = unify left_type int_typ in
         let* subst'' = unify right_type int_typ in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         return (final_subst, bool_typ, env)
       | Eq | Neq ->
         let* fresh_eq_var = fresh_eq_var in
         let* subst' = unify left_type fresh_eq_var in
         let* subst'' = unify right_type fresh_eq_var in
         let* final_subst =
           Subst.compose_all [ left_subst; right_subst; subst'; subst'' ]
         in
         return (final_subst, bool_typ, env)
       | And | Or ->
         let* subst1 = unify left_type bool_typ in
         let* subst2 = unify right_type bool_typ in
         let* final_subst =
           Subst.compose_all [ subst1; subst2; left_subst; right_subst ]
         in
         return (final_subst, bool_typ, env))
    | ETuple list ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_type, _ = ehelper env head in
          let* subst' = Subst.compose subst head_subst in
          let* final_subst, tail_type = subst_tuple subst' tail in
          return (final_subst, head_type :: tail_type)
      in
      let* final_subst, typ_list = subst_tuple Subst.empty list in
      return (final_subst, tuple_t @@ List.map typ_list ~f:(Subst.apply final_subst), env)
    | ECons (elem, list) ->
      let* elem_subst, elem_type, _ = ehelper env elem in
      let* list_subst, list_type, _ = ehelper env list in
      let* subst' = unify (list_t elem_type) list_type in
      let* final_subst = Subst.compose_all [ elem_subst; list_subst; subst' ] in
      return (final_subst, Subst.apply subst' (list_t elem_type), env)
    | EMatch (matched_expression, case_list) ->
      let* matched_subst, matched_type, _ = ehelper env matched_expression in
      let head = List.hd_exn case_list in
      let bootstrap_env env case =
        let identifiers = find_identifiers case in
        Set.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
          let* fresh_var = fresh_var in
          let* acc = acc in
          return (TypeEnv.extend acc id (Set.empty (module Int), fresh_var)))
      in
      let* env' = bootstrap_env env (fst head) in
      let* head_subst, head_expression_type, _ = ehelper env' (snd head) in
      let* subst' =
        List.fold_right case_list ~init:(return Subst.empty) ~f:(fun case subst ->
          let* env'' = bootstrap_env env' (fst case) in
          let* case_subst, case_type = pthelper env'' (fst case) in
          let* subst'' = unify case_type matched_type in
          let* computation_subst, computation_type, _ = ehelper env'' (snd case) in
          let* subst''' = unify computation_type head_expression_type in
          let* subst = subst in
          Subst.compose_all
            [ subst'''; subst''; subst; case_subst; computation_subst; head_subst ])
      in
      let* final_subst = Subst.compose subst' matched_subst in
      return (subst', Subst.apply final_subst head_expression_type, env)
    | ELet (bindings_list, expression) ->
      let rec process_list subst env = function
        | [] -> return (subst, env)
        | elem :: tail ->
          let* identifier, exp =
            match elem with
            | _, PtVar id, exp -> return (id, exp)
            | _ -> fail `Unreachable
          in
          let* fresh_var = fresh_val in
          let env' =
            TypeEnv.extend env identifier (Base.Set.empty (module Base.Int), fresh_var)
          in
          let* elem_subst, elem_typ, _ = ehelper env' exp in
          let env'' = TypeEnv.apply elem_subst env' in
          let generalized_type = generalize env'' elem_typ in
          let* subst'' = Subst.compose subst elem_subst in
          process_list subst'' (TypeEnv.extend env'' identifier generalized_type) tail
      in
      let* subst', env' = process_list Subst.empty env bindings_list in
      let* subst_expr, typ_expr, _ = ehelper env' expression in
      let* final_subst = Subst.compose subst' subst_expr in
      return (final_subst, typ_expr, env)
    | EApp (left, right) ->
      let* fn_name = get_name left in
      let* subst_left, typ_left, _ = ehelper env left in
      let* subst_right, typ_right, _ = ehelper (TypeEnv.apply subst_left env) right in
      let* type_variable = fresh_var in
      let* subst' =
        unify (arrow_t typ_right type_variable) (Subst.apply subst_right typ_left)
      in
      let result_type = Subst.apply subst' type_variable in
      let get_typ = function
        | Ok (_, typ, _) -> return typ
        | _ -> fail `Unreachable
      in
      let rec construct_typ acc = function
        | EApp (l, r) ->
          let* next_typ = get_typ @@ run @@ ehelper (TypeEnv.apply subst_left env) r in
          construct_typ (TArr (acc, next_typ)) l
        | _ -> return @@ TArr (acc, result_type)
      in
      let* new_typ' = construct_typ typ_right left in
      let* final_subst = Subst.compose_all [ subst_left; subst_right; subst' ] in
      let new_env' =
        TypeEnv.extend env fn_name (Set.empty (module Int), restrict new_typ')
      in
      let new_env = if is_restricted typ_left then new_env' else env in
      return (final_subst, result_type, new_env)
    | EFun (PtVar head, body) ->
      let* type_variable = fresh_var in
      let env' = TypeEnv.extend env head (Set.empty (module Int), type_variable) in
      let* subst, typ, _ = ehelper env' body in
      let result_type = arrow_t (Subst.apply subst type_variable) typ in
      return (subst, result_type, env)
    | EFun (PtWild, body) ->
      let* type_variable = fresh_var in
      let* subst, typ, _ = ehelper env body in
      return (subst, arrow_t type_variable typ, env)
    | EFun (_, _) -> fail `Unreachable
    | EIf (condition, true_branch, false_branch) ->
      let* condition_subst, condition_type, _ = ehelper env condition in
      let* true_branch_subst, true_branch_type, _ = ehelper env true_branch in
      let* false_branch_subst, false_branch_type, _ = ehelper env false_branch in
      let* subst' = unify condition_type bool_typ in
      let* subst'' = unify false_branch_type true_branch_type in
      let* final_subst =
        Subst.compose_all
          [ condition_subst; subst'; subst''; true_branch_subst; false_branch_subst ]
      in
      return (final_subst, Subst.apply final_subst false_branch_type, env)
    | ENone ->
      let* content_subst, content_typ =
        let* fresh_var = fresh_var in
        return (Subst.empty, fresh_var)
      in
      return (content_subst, option_t content_typ, env)
    | ESome x ->
      let* subst, ttyp, _ = ehelper env x in
      return (subst, option_t ttyp, env)
  in
  ehelper
;;

let check_types environment (dec : decl) =
  match dec with
  | DLet (false, PtVar name, exp) ->
    let* subst, function_type, environment' = infer environment exp in
    let res_typ = check_restrict exp function_type in
    let generalized_type = generalize environment' (Subst.apply subst res_typ) in
    return (TypeEnv.extend environment' name generalized_type, res_typ)
  | DLet (true, PtVar name, exp) ->
    let* type_variable = fresh_var in
    let env =
      TypeEnv.extend environment name (Base.Set.empty (module Base.Int), type_variable)
    in
    let* subst, typ', environment' = infer env exp in
    let typ = check_restrict exp typ' in
    let* subst' = unify (Subst.apply subst type_variable) typ in
    let* final_subst = Subst.compose subst' subst in
    let env = TypeEnv.apply final_subst env in
    let generalized_type = generalize env (Subst.apply final_subst type_variable) in
    return (TypeEnv.extend environment' name generalized_type, typ)
  | _ -> fail `Unreachable
;;

let run_inference expression env = run (check_types env expression)
