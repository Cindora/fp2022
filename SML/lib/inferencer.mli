(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module R : sig
  type 'a t
end

type fresh = int

module Subst : sig
  type t
end

module TypeEnv : sig
  type t = (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t

  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
end

val run_inference
  :  Ast.decl
  -> TypeEnv.t
  -> ( (string, Typing.scheme, Base.String.comparator_witness) Base.Map.t * Typing.typ
     , Typing.error )
     result
