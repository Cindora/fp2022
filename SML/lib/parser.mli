(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val parse : 'a Angstrom.t -> string -> ('a, string) result
val prog : Ast.prog Angstrom.t
