(** Copyright 2022-2023, Nikita Olkhovsky *)

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
  | ELiteral of b_liter (* 123 *)
  | EIdentifier of name (* xs *)
  | EUnaryOp of unary_op * expr (* ~1 *)
  | EBinaryOp of binary_op * expr * expr (* 2 + 2 *)
  | ETuple of expr list (* (1, '2', "3") *)
  | EList of expr list (* [1, 2] *)
  | EConsList of expr * expr (* 1 :: [2] *)
[@@deriving show { with_path = false }]

(* Конструкторы выражений *)
let e_b_expr x = ELiteral x
let e_identifier x = EIdentifier x
let e_unary_op op x = EUnaryOp (op, x)
let e_binary_op op left right = EBinaryOp (op, left, right)
let e_tuple elements = ETuple elements
let e_list elements = EList elements
let e_cons_list head tail = EConsList (head, tail)

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
