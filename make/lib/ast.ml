(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type substring =
  | STR of string
  | VAR of string
[@@deriving show { with_path = false }]

type command = COMMAND of substring list [@@deriving show { with_path = false }]

type rule =
  { targets : string
  ; prerequisites : string
  ; commands : substring list list
  }
[@@deriving show { with_path = false }]

type expr =
  | RULE of rule
  | VAR_DEC of string * string
[@@deriving show { with_path = false }]

type ast = expr list [@@deriving show { with_path = false }]
