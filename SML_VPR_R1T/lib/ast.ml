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
type unary_op =
  | Neg (* ~ *)
  | Not (* not *)
[@@deriving show { with_path = false }]

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
