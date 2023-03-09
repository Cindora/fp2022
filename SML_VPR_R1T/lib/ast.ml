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
  | LeEq (* <= *)
  | Grt (* > *)
  | GrEq (* >= *)
  | And
  | Or
[@@deriving show { with_path = false }]

(* Выражения *)
type expr =
  | XLiteral of b_liter (* 123 *)
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
let e_b_expr x = XLiteral x
let e_identifier x = XIdentifier x
let e_unary_op op x = XUnaryOp (op, x)
let e_binary_op op left right = XBinaryOp (op, left, right)
let e_tuple elements = XTuple elements
let e_list elements = XList elements
let e_cons_list head tail = XConsList (head, tail)
let e_case_of expression cases = XCaseOf (expression, cases)
let e_let_in declarations body = XLetIn (declarations, body)
let e_application func args = XApplication (func, args)
let e_val_dec value_id expression = XValDec (value_id, expression)
let e_val_rec_dec value_id expression = XValRecDec (value_id, expression)
let e_arrow_fun args_id expression = XArrowFun (args_id, expression)
let e_if_then_else cond if_true if_false = XIfThenElse (cond, if_true, if_false)

(* Конструкторы операторов *)
let uneg _ = Neg
let unot _ = Not
let a_add _ = Add
let a_sub _ = Sub
let a_mul _ = Mult
let a_div _ = Div
let a_eq _ = Eq
let a_neq _ = NotEq
let a_ls _ = Less
let a_lse _ = LeEq
let a_gt _ = Grt
let a_gte _ = GrEq
let a_and _ = And
let a_or _ = Or
