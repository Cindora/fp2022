(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD_ERROR = sig
  type 'a t = ('a, error) result

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
end

module Environment (M : MONAD_ERROR) = struct
  open M

  let find map k =
    match Base.Map.find map k with
    | Some v -> return v
    | None -> fail `ValueUnbound
  ;;

  let update env k value = Base.Map.update env k ~f:(fun _ -> value)
  let empty = Base.Map.empty (module Base.String)
end

module Interpret (M : MONAD_ERROR) : sig
  val run : expr -> value M.t
end = struct
  open M
  open Environment (M)

  let rec eval (expr : expr) (env : env) =
    let rec foldr f ini = function
      | [] -> return ini
      | h :: tl ->
        let* h = eval h env in
        let* tl = foldr f ini tl in
        return (f h tl)
    in
    match expr with
    | XLiteral literal ->
      (match literal with
       | BaseInt l -> return (ValInt l)
       | BaseString l -> return (ValString l)
       | BaseBool l -> return (ValBool l)
       | BaseChar l -> return (ValChar l)
       | BaseUnit -> return ValUnit)
    | XBinaryOp (operation, left_op, right_op) ->
      let* left_op = eval left_op env in
      let* right_op = eval right_op env in
      (match operation, left_op, right_op with
       (* Add, Sub, Mult, Div *)
       | Add, ValInt l, ValInt r -> return (ValInt (l + r))
       | Sub, ValInt l, ValInt r -> return (ValInt (l - r))
       | Mult, ValInt l, ValInt r -> return (ValInt (l * r))
       | Div, ValInt _, ValInt y when y = 0 -> fail `DivisionByZero
       | Div, ValInt l, ValInt r -> return (ValInt (l / r))
       | (Add | Sub | Mult | Div), _, _ -> fail `Unreachable
       (* And, Or *)
       | And, ValBool l, ValBool r -> return (ValBool (l && r))
       | Or, ValBool l, ValBool r -> return (ValBool (l || r))
       | (And | Or), _, _ -> fail `Unreachable
       (* Eq, NotEq, Greater, GreaterEq, Less, LessEq *)
       | Eq, ValInt l, ValInt r -> return (ValBool (l == r))
       | Eq, ValString l, ValString r -> return (ValBool (l == r))
       | Eq, ValBool l, ValBool r -> return (ValBool (l == r))
       | Eq, ValTuple l, ValTuple r -> return (ValBool (l == r))
       | Eq, ValList l, ValList r -> return (ValBool (l == r))
       | NotEq, ValInt l, ValInt r -> return (ValBool (l != r))
       | NotEq, ValString l, ValString r -> return (ValBool (l != r))
       | NotEq, ValBool l, ValBool r -> return (ValBool (l != r))
       | NotEq, ValTuple l, ValTuple r -> return (ValBool (l != r))
       | NotEq, ValList l, ValList r -> return (ValBool (l != r))
       | Grt, ValInt l, ValInt r -> return (ValBool (l > r))
       | Grt, ValString l, ValString r -> return (ValBool (l > r))
       | Grt, ValTuple l, ValTuple r -> return (ValBool (l > r))
       | Grt, ValList l, ValList r -> return (ValBool (l > r))
       | GrtEq, ValInt l, ValInt r -> return (ValBool (l >= r))
       | GrtEq, ValString l, ValString r -> return (ValBool (l >= r))
       | GrtEq, ValTuple l, ValTuple r -> return (ValBool (l >= r))
       | GrtEq, ValList l, ValList r -> return (ValBool (l >= r))
       | Less, ValInt l, ValInt r -> return (ValBool (l < r))
       | Less, ValString l, ValString r -> return (ValBool (l < r))
       | Less, ValTuple l, ValTuple r -> return (ValBool (l < r))
       | Less, ValList l, ValList r -> return (ValBool (l < r))
       | LessEq, ValInt l, ValInt r -> return (ValBool (l <= r))
       | LessEq, ValString l, ValString r -> return (ValBool (l <= r))
       | LessEq, ValTuple l, ValTuple r -> return (ValBool (l <= r))
       | LessEq, ValList l, ValList r -> return (ValBool (l <= r))
       | _, _, _ -> fail `OperationUnsupport)
    | XIdentifier identifier ->
      let* v = find env identifier in
      (match v with
       | ValFun (identifier_list, body, env, Rec) ->
         return (ValFun (identifier_list, body, update env identifier v, Rec))
       | _ -> return v)
    | XApplication (func, arg) ->
      let* eval_argument = eval arg env in
      let* eval_function = eval func env in
      let* identifier_list, body, env, recursive =
        match eval_function with
        | ValFun (identifier_list, body, env, recursive) ->
          return (identifier_list, body, env, recursive)
        | _ -> fail `Unreachable
      in
      let* id, identifier_list =
        match identifier_list with
        | head :: tail -> return (head, tail)
        | _ -> fail `Unreachable
      in
      let env =
        match id with
        | "_" -> env
        | _ -> update env id eval_argument
      in
      (match identifier_list with
       | [] -> eval body env
       | _ -> return (ValFun (identifier_list, body, env, recursive)))
    | XArrowFun (arguments_list, body) ->
      (match arguments_list with
       | [] -> eval body env
       | _ -> return (ValFun (arguments_list, body, env, NRec)))
    | XValDec (_, body) -> eval body env
    | XValRecDec (_, body) ->
      let* eval_body = eval body env in
      (match eval_body with
       | ValFun (identifier_list, body, env, _) ->
         return (ValFun (identifier_list, body, env, Rec))
       | _ -> return eval_body)
    | XIfThenElse (if_expr, then_expr, else_expr) ->
      let* eval_conditional = eval if_expr env in
      (match eval_conditional with
       | ValBool true -> eval then_expr env
       | ValBool false -> eval else_expr env
       | _ -> fail `Unreachable)
    | XUnaryOp (op, liter) ->
      let* liter = eval liter env in
      (match op, liter with
       | Neg, ValInt l -> return (ValInt (-l))
       | Not, ValBool l -> return (ValBool (not l))
       | _ -> fail `Unreachable)
    | XList expr_list ->
      (match expr_list with
       | [] -> return (ValList [])
       | _ ->
         let rec eval_list expr_list =
           match Base.List.hd_exn expr_list, Base.List.tl_exn expr_list with
           | hd, [] ->
             let* head = eval hd env in
             return (ValList [ head ])
           | hd, tl ->
             let* head = eval hd env in
             let* tail = eval_list tl in
             (match tail with
              | ValList tl -> return (ValList (head :: tl))
              | _ -> fail `Unreachable)
         in
         eval_list expr_list)
    | XConsList (op, expr_list) ->
      let* operand = eval op env in
      let* expr_list = eval expr_list env in
      (match operand, expr_list with
       | l, ValList expr_list -> return (ValList (l :: expr_list))
       | _ -> fail `Unreachable)
    | XTuple expr_list ->
      let* expr_list = foldr (fun l ls -> l :: ls) [] expr_list in
      return (ValTuple expr_list)
    | XLetIn (bind_list, expr) ->
      let rec eval_bindings env = function
        | h :: t ->
          let* result = eval h env in
          (match h with
           | XValDec (name, _) | XValRecDec (name, _) ->
             eval_bindings (update env name result) t
           | _ -> fail `Unreachable)
        | _ -> eval expr env
      in
      eval_bindings env bind_list
    | XCaseOf (case_expr, of_expr) ->
      let* env = return env in
      let rec patterns_compare case_expr case action env =
        let rec helper env = function
          | matched_hd :: matched_tl, hd :: tl ->
            let is_suit, env, hd_success =
              patterns_compare matched_hd hd (XArrowFun ([ "_" ], XLiteral BaseUnit)) env
            in
            let monadic_execution, curr_env, tl_success = helper env (matched_tl, tl) in
            is_suit *> monadic_execution, curr_env, hd_success && tl_success
          | [], [] -> eval action env, env, true
          | _ -> fail `PatternMatchingFail, env, false
        in
        match case_expr, case with
        | ValInt value, XLiteral (BaseInt l) when value = l -> eval action env, env, true
        | ValChar value, XLiteral (BaseChar l) when value = l ->
          eval action env, env, true
        | ValBool value, XLiteral (BaseBool l) when value = l ->
          eval action env, env, true
        | ValString value, XLiteral (BaseString l) when value = l ->
          eval action env, env, true
        | ValUnit, XLiteral BaseUnit -> eval action env, env, true
        | ValInt value, XUnaryOp (Neg, XLiteral (BaseInt l)) when value = -l ->
          eval action env, env, true
        | value, XIdentifier id ->
          let curr_env =
            match id with
            | "_" -> env
            | _ -> update env id value
          in
          eval action curr_env, curr_env, true
        | ValList m_list, XList list -> helper env (m_list, list)
        | ValTuple m_tuple, XTuple tuple -> helper env (m_tuple, tuple)
        | ValList m_list, XConsList (hd, tl) ->
          (match m_list with
           | matched_hd :: matched_tl ->
             let is_suit, env, hd_success =
               patterns_compare matched_hd hd (XArrowFun ([ "_" ], XLiteral BaseUnit)) env
             in
             let monadic_execution, curr_env, tail_success =
               patterns_compare (ValList matched_tl) tl action env
             in
             is_suit *> monadic_execution, curr_env, hd_success && tail_success
           | [] -> fail `PatternMatchingFail, env, false)
        | _ -> fail `PatternMatchingFail, env, false
      in
      let* eval_matched_expr = eval case_expr env in
      let rec helper = function
        | hd :: tl ->
          let is_suit, _, success =
            patterns_compare eval_matched_expr (fst hd) (snd hd) env
          in
          if success then is_suit else helper tl
        | [] -> fail `PatternMatchingIncomplete
      in
      helper of_expr
  ;;

  let run expr = eval expr empty
end

open Interpret (struct
  type 'a t = ('a, error) result

  let return = Stdlib.Result.ok
  let fail = Stdlib.Result.error

  let ( >>= ) l_expr r_expr =
    match l_expr with
    | Ok l -> r_expr l
    | Error s -> Error s
  ;;

  let ( let* ) l_expr r_expr =
    match l_expr with
    | Ok l -> r_expr l
    | Error s -> Error s
  ;;

  let ( *> ) l r = l >>= fun _ -> r
end)

let interpret input =
  match Parser.parse_strings input with
  | Error msg -> Format.fprintf Format.std_formatter "Parse error: (%S)" msg
  | Ok ast ->
    (match run ast with
     | Ok result -> print_value result
     | Error err -> print_error err)
;;
