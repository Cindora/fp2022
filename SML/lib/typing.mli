(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int
type identifier = string

type ground_type =
  | String
  | Int
  | Bool

type typ =
  | TVar of type_variable_number * bool
  | TEqualityVar of type_variable_number * bool
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TOption of typ
  | TGround of ground_type

val string_typ : typ
val int_typ : typ
val bool_typ : typ
val var_t : type_variable_number -> typ
val eqvar_t : type_variable_number -> typ
val val_t : type_variable_number -> typ
val eqval_t : type_variable_number -> typ
val arrow_t : typ -> typ -> typ
val tuple_t : typ list -> typ
val option_t : typ -> typ
val list_t : typ -> typ

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `NoVariable of identifier
  | `Not_function
  | `OccursCheck
  | `UnificationFailed of typ * typ
  | `Unreachable
  ]

val pp_type : Format.formatter -> typ -> unit
val print_typ : typ -> unit
val pp_error : Format.formatter -> error -> unit
val print_type_error : error -> unit
