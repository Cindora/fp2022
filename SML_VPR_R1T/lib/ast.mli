(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type b_liter =
  | BaseChar of char
  | BaseString of string
  | BaseInt of int
  | BaseBool of bool
  | BaseUnit
[@@deriving show { with_path = false }]

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

type expr =
  | ELiteral of b_liter (* 123 *)
  | EIdentifier of name (* var *)
  | EBinaryOp of binary_op * expr * expr (* 2 + 2 *)
  | ETuple of expr list (* (1, '2', "3") *)
  | EList of expr list (* [1, 2] *)
  | EConsList of expr * expr (* 1 :: [2] *)
[@@deriving show { with_path = false }]
