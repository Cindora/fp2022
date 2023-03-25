(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Typing

type error = Typing.error

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  (int, 'a, Int.comparator_witness) Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
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
    let ( let+ ) x f = bind x ~f:(fun x -> return (f x))
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
  type t = type_expr

  let rec occurs_in v = function
    | TVar id -> id = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> List.exists typ_list ~f:(occurs_in v)
    | TList type_expr -> occurs_in v type_expr
    | TPrime _ -> false
  ;;

  let free_vars =
    let empty_set = Set.empty (module Int) in
    let rec helper acc = function
      | TVar n -> Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple typ_list ->
        List.fold_right
          typ_list
          ~f:(fun t s -> Set.union s (helper empty_set t))
          ~init:acc
      | TList type_expr -> helper acc type_expr
      | TPrime _ -> acc
    in
    helper empty_set
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> type_expr -> t R.t

  (** Getting value from substitution *)
  val find_exn : fresh -> t -> type_expr

  val find : fresh -> t -> type_expr option
  val apply : t -> type_expr -> type_expr
  val unify : type_expr -> type_expr -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* An association list. In real world replace it by Map *)
  type t = (fresh, type_expr, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping key value =
    if Type.occurs_in key value then fail `OccursCheck else return (key, value)
  ;;

  let singleton key value =
    let+ key, value = mapping key value in
    Map.update empty key ~f:(fun _ -> value)
  ;;

  let find_exn key subst = Map.find_exn subst key
  let find key subst = Map.find subst key
  let remove subst key = Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find_exn n s with
         | exception Not_found_s _ -> var_t n
         | x -> x)
      | TArr (left, right) -> arrow_t (helper left) (helper right)
      | TTuple typ_list -> tuple_t (List.map typ_list ~f:helper)
      | TList type_expr -> list_t (helper type_expr)
      | prime -> prime
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrime l, TPrime r when l == r -> return empty
    | TPrime _, TPrime _ -> fail (`UnificationFailed (l, r))
    | TVar a, TVar b when a = b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArr (l1, r1), TArr (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match List.zip typ_list_l typ_list_r with
       | List.Or_unequal_lengths.Unequal_lengths -> fail (`UnificationFailed (l, r))
       | List.Or_unequal_lengths.Ok zipped_list ->
         List.fold_right
           zipped_list
           ~f:(fun (x, y) subst ->
             let* head_sub = unify x y in
             let* subst = subst in
             compose head_sub subst)
           ~init:(return empty))
    | TList typ1, TList typ2 -> unify typ1 typ2
    | _ -> fail (`UnificationFailed (l, r))

  and extend k v s =
    match find k s with
    | None ->
      let* s2 = singleton k (apply s v) in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let+ k, v = mapping k (apply s2 v) in
        Map.update acc k ~f:(fun _ -> v))
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
  let fold_right f ini set =
    Set.fold_right set ~init:ini ~f:(fun x acc ->
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

  let free_vars = function
    | s, t -> Set.diff (Type.free_vars t) s
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

  let free_vars : t -> (type_variable_number, Int.comparator_witness) Set.t =
    Map.fold
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn ~equal:String.equal map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| var_t

let instantiate : scheme -> type_expr R.t =
 fun (set, t) ->
  VarSet.fold_right
    (fun type_expr name ->
      let* f = fresh_var in
      let+ s = Subst.singleton name f in
      Subst.apply s type_expr)
    (return t)
    set
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env type_expr ->
  let free = Set.diff (Type.free_vars type_expr) (TypeEnv.free_vars env) in
  free, type_expr
;;

let lookup_env e map =
  match Map.find map e with
  | None -> fail (`NoVariable e)
  | Some scheme ->
    let+ ans = instantiate scheme in
    Subst.empty, ans
;;

let rec find_identifiers = function
  | XBinaryOp (_, left, right) ->
    Set.union (find_identifiers left) (find_identifiers right)
  | XUnaryOp (_, operand) -> find_identifiers operand
  | XApplication (expr1, expr2) ->
    Set.union (find_identifiers expr1) (find_identifiers expr2)
  | XList expr_list | XTuple expr_list ->
    List.fold_right
      ~f:(fun expression acc -> Set.union (find_identifiers expression) acc)
      ~init:(Set.empty (module String))
      expr_list
  | XConsList (head, tail) -> Set.union (find_identifiers head) (find_identifiers tail)
  | XIdentifier id -> Set.singleton (module String) id
  | _ -> Set.empty (module String)
;;

let infer =
  let rec helper : TypeEnv.t -> expr -> (Subst.t * type_expr) R.t =
   fun env -> function
    | XLiteral literal ->
      (match literal with
       | BaseInt _ -> return (Subst.empty, int_typ)
       | BaseString _ -> return (Subst.empty, string_typ)
       | BaseChar _ -> return (Subst.empty, char_typ)
       | BaseBool _ -> return (Subst.empty, bool_typ)
       | BaseUnit -> return (Subst.empty, unit_typ))
    | XIdentifier identifier ->
      (match identifier with
       | "_" ->
         let+ fresh_var = fresh_var in
         Subst.empty, fresh_var
       | _ -> lookup_env identifier env)
    | XUnaryOp (op, expr) ->
      let operand_type =
        match op with
        | Neg -> int_typ
        | Not -> bool_typ
      in
      let* subst, t = helper env expr in
      let* subst' = unify t operand_type in
      let+ final_subst = Subst.compose subst' subst in
      final_subst, operand_type
    | XBinaryOp (op, left, right) ->
      let* left_subst, left_type = helper env left in
      let* right_subst, right_type = helper env right in
      (match op with
       | Add | Sub | Mult | Div ->
         let* subst' = unify left_type int_typ in
         let* subst'' = unify right_type int_typ in
         let+ final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         final_subst, int_typ
       | Eq | NotEq | Grt | GrtEq | Less | LessEq ->
         let* fresh_eq_var = fresh_var in
         let* subst' = unify left_type fresh_eq_var in
         let* subst'' = unify right_type fresh_eq_var in
         let+ final_subst =
           Subst.compose_all [ left_subst; right_subst; subst'; subst'' ]
         in
         final_subst, bool_typ
       | And | Or ->
         let* subst' = unify left_type bool_typ in
         let* subst'' = unify right_type bool_typ in
         let+ final_subst =
           Subst.compose_all [ subst'; subst''; left_subst; right_subst ]
         in
         final_subst, bool_typ)
    | XTuple list ->
      let rec subst_tuple subst = function
        | [] -> return (subst, [])
        | head :: tail ->
          let* head_subst, head_type = helper env head in
          let* subst' = Subst.compose subst head_subst in
          let+ final_subst, tail_type = subst_tuple subst' tail in
          final_subst, head_type :: tail_type
      in
      let+ final_subst, typ_list = subst_tuple Subst.empty list in
      final_subst, tuple_t (List.map typ_list ~f:(Subst.apply final_subst))
    | XList list ->
      (match list with
       | [] ->
         let+ fresh_var = fresh_var in
         Subst.empty, TList fresh_var
       | head :: tail ->
         let* head_subst, head_type = helper env head in
         let rec substlist subst = function
           | [] -> return subst
           | elem :: tail ->
             let* elem_subst, elem_type = helper env elem in
             let* subst' = unify elem_type head_type in
             let* subst'' = Subst.compose_all [ subst; elem_subst; subst' ] in
             substlist subst'' tail
         in
         let+ final_subst = substlist head_subst tail in
         final_subst, list_t (Subst.apply final_subst head_type))
    | XConsList (elem, list) ->
      let* elem_subst, elem_type = helper env elem in
      let* list_subst, list_type = helper env list in
      let* subst' = unify (list_t elem_type) list_type in
      let+ final_subst = Subst.compose_all [ elem_subst; list_subst; subst' ] in
      final_subst, Subst.apply subst' list_type
    | XCaseOf (matched_expression, case_list) ->
      let* matched_subst, matched_type = helper env matched_expression in
      let head = List.hd_exn case_list in
      let bootstrap_env env case =
        let identifiers = find_identifiers case in
        Set.fold_right identifiers ~init:(return env) ~f:(fun id acc ->
          let* fresh_var = fresh_var in
          let+ acc = acc in
          TypeEnv.extend acc id (Set.empty (module Int), fresh_var))
      in
      let* env' = bootstrap_env env (fst head) in
      let* _, head_expression_type = helper env' (snd head) in
      let* subst' =
        List.fold_right case_list ~init:(return Subst.empty) ~f:(fun case subst ->
          let* env'' = bootstrap_env env (fst case) in
          let* case_subst, case_type = helper env'' (fst case) in
          let* subst'' = unify case_type matched_type in
          let* computation_subst, computation_type = helper env'' (snd case) in
          let* subst''' = unify computation_type head_expression_type in
          let* subst = subst in
          Subst.compose_all [ subst'''; subst''; subst; case_subst; computation_subst ])
      in
      let+ final_subst = Subst.compose subst' matched_subst in
      subst', Subst.apply final_subst head_expression_type
    | XLetIn (bindings_list, expression) ->
      let rec process_list subst env = function
        | [] -> return (subst, env)
        | elem :: tail ->
          let* identifier, _ =
            match elem with
            | XValDec (id, body) | XValRecDec (id, body) -> return (id, body)
            | _ -> fail `Binding
          in
          let* fresh_var = fresh_var in
          let env' = TypeEnv.extend env identifier (Set.empty (module Int), fresh_var) in
          let* elem_subst, elem_type = helper env' elem in
          let env'' = TypeEnv.apply elem_subst env' in
          let generalized_type = generalize env'' elem_type in
          let* subst' = Subst.compose subst elem_subst in
          let env''' = TypeEnv.extend env'' identifier generalized_type in
          process_list subst' env''' tail
      in
      let* subst', env' = process_list Subst.empty env bindings_list in
      let* subst_expr, typ_expr = helper env' expression in
      let+ final_subst = Subst.compose subst' subst_expr in
      final_subst, typ_expr
    | XApplication (left, right) ->
      let* left_subst, left_type = helper env left in
      let* right_subst, right_type = helper (TypeEnv.apply left_subst env) right in
      let* type_variable = fresh_var in
      let* subst' =
        unify (arrow_t right_type type_variable) (Subst.apply right_subst left_type)
      in
      let result_type = Subst.apply subst' type_variable in
      let+ final_subst = Subst.compose_all [ left_subst; right_subst; subst' ] in
      final_subst, result_type
    | XValDec (_, body) | XValRecDec (_, body) -> helper env (XArrowFun ([], body))
    | XArrowFun (args, body) ->
      (match args with
       | [] -> helper env body
       | head :: tail ->
         let* type_variable = fresh_var in
         let env' = TypeEnv.extend env head (Set.empty (module Int), type_variable) in
         let+ subst, type_expr = helper env' (XArrowFun (tail, body)) in
         let result_type = arrow_t (Subst.apply subst type_variable) type_expr in
         subst, result_type)
    | XIfThenElse (condition, true_branch, false_branch) ->
      let* condition_subst, condition_type = helper env condition in
      let* true_branch_subst, true_branch_type = helper env true_branch in
      let* false_branch_subst, false_branch_type = helper env false_branch in
      let* subst' = unify condition_type bool_typ in
      let* subst'' = unify true_branch_type false_branch_type in
      let+ final_subst =
        Subst.compose_all
          [ condition_subst; true_branch_subst; false_branch_subst; subst'; subst'' ]
      in
      final_subst, Subst.apply final_subst true_branch_type
  in
  helper
;;

let run_inference expression = Result.map (run (infer TypeEnv.empty expression)) ~f:snd

let parse_and_inference input =
  match Parser.parse_strings input with
  | Ok ast ->
    (match run_inference ast with
     | Ok type_expr -> print_type_expr type_expr
     | Error e -> print_type_expre_error e)
  | Error e -> Format.fprintf Format.std_formatter "Parsing error: (%S)" e
;;
