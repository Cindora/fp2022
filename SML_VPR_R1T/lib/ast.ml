(** Copyright 2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

(* Базовые типы *)
type b_liter =
  | BaseChar of char
  | BaseString of string
  | BaseInt of int
  | BaseBool of bool
  | BaseUnit
[@@deriving show { with_path = false }]

(* Операторы *)
and unary_op =
  | Neg (* ~ *)
  | Not (* not *)

type binary_op =
  | Add
  | Sub
  | Mult
  | Div
  | Eq (* = *)
  | NotEq (* <> *)
  | Less (* < *)
  | LessEq (* <= *)
  | Grt (* > *)
  | GrtEq (* >= *)
  | And
  | Or
[@@deriving show { with_path = false }]

(* Выражения *)
type expr =
  | XLiteral of b_liter (* 123 abc true *)
  | XIdentifier of name (* xs *)
  | XUnaryOp of unary_op * expr (* ~1 *)
  | XBinaryOp of binary_op * expr * expr (* 2 + 2 *)
  | XTuple of expr list (* (expr, expr, expr) *)
  | XList of expr list (* [expr, expr] *)
  | XConsList of expr * expr (* expr :: [expr, expr] *)
  | XCaseOf of expr * (expr * expr) list
    (* case xs of
        [] => 0 
      | x::_ => x *)
  | XLetIn of expr list * expr (* let val a = 1 val b = 2 in a + b end *)
  | XApplication of expr * expr (* f x: ident * liter *)
  | XValDec of name * expr (* val ident = liter *)
  | XValRecDec of name * expr
    (* val rec factorial = fn n => if n <= 1 then 1 else n * factorial (n - 1) *)
  | XArrowFun of name list * expr (* fn x => x + 1 *)
  | XIfThenElse of expr * expr * expr (* if true then 1 else 0 *)
[@@deriving show { with_path = false }]

(* Конструкторы выражений *)
let expr_b_expr b = XLiteral b
let expr_identifier b = XIdentifier b
let expr_unary_op op b = XUnaryOp (op, b)
let expr_binary_op op left_expr right_expr = XBinaryOp (op, left_expr, right_expr)
let expr_tuple el = XTuple el
let expr_list el = XList el
let expr_cons_list head tail = XConsList (head, tail)
let expr_casexpr_of expr case_expr = XCaseOf (expr, case_expr)
let expr_let_in let_expr in_expr = XLetIn (let_expr, in_expr)
let expr_application func_expr args = XApplication (func_expr, args)
let expr_val_dec value_expr expr = XValDec (value_expr, expr)
let expr_val_rec_dec value_expr expr = XValRecDec (value_expr, expr)
let expr_arrow_fun args expr = XArrowFun (args, expr)

let expr_if_then_else if_expr then_expr else_expr =
  XIfThenElse (if_expr, then_expr, else_expr)
;;

(* Конструкторы операторов *)
let u_neg _ = Neg
let u_not _ = Not
let a_add _ = Add
let a_sub _ = Sub
let a_mult _ = Mult
let a_div _ = Div
let a_eq _ = Eq
let a_neq _ = NotEq
let a_less _ = Less
let a_lesseq _ = LessEq
let a_grt _ = Grt
let a_grteq _ = GrtEq
let a_and _ = And
let a_or _ = Or

type env = (name, value, Base.String.comparator_witness) Base.Map.t

and is_rec =
  | Rec
  | NRec

and value =
  | ValInt of int
  | ValString of string
  | ValBool of bool
  | ValChar of char
  | ValUnit
  | ValList of value list
  | ValTuple of value list
  | ValFun of name list * expr * env * is_rec

type error =
  [ `ValueUnbound
  | `Unreachable
  | `OperationUnsupport
  | `DivisionByZero
  | `WildcardMisuse
  | `PatternMatchingFail
  | `PatternMatchingIncomplete
  ]

let rec pp_value fmtr =
  let open Format in
  let pp_list fmt delimiter =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
      (fun fmt value -> pp_value fmt value)
      fmt
  in
  function
  | ValInt value -> fprintf fmtr "%d" value
  | ValChar value -> fprintf fmtr "%C" value
  | ValBool value -> fprintf fmtr "%B" value
  | ValString value -> fprintf fmtr "%S" value
  | ValUnit -> fprintf fmtr "()"
  | ValList list -> fprintf fmtr "[%a]" (fun fmt -> pp_list fmt ", ") list
  | ValTuple tuple -> fprintf fmtr "(%a)" (fun fmt -> pp_list fmt ", ") tuple
  | ValFun _ -> fprintf fmtr "fn"
;;

let print_value = Format.printf "%a\n" pp_value

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `ValueUnbound -> fprintf fmt "Ошибка присваивания значения."
  | `Unreachable -> fprintf fmt "Данный код недоступен. "
  | `OperationUnsupport -> fprintf fmt "Неподдерживаемая операция."
  | `DivisionByZero -> fprintf fmt "Деление на ноль."
  | `WildcardMisuse -> fprintf fmt "Ошибочное использование подстановочного знака."
  | `PatternMatchingFail -> fprintf fmt "Pattern Matching завершился с ошибкой."
  | `PatternMatchingIncomplete ->
    fprintf fmt "Pattern Matсhing не является исчерпывающим."
;;

let print_error = Format.printf "%a" pp_error
