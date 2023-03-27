(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Make_lib

let exprs_pp input =
  match Parser.parser input with
  | Result.Ok res -> Format.printf "%a\n%!\n" Ast.pp_ast res
  | Error e -> print_string e
;;

let () =
  (*Stdlib.Sys.chdir "demo_project";*)
  let input = Core.In_channel.read_all "./Makefile" in
  exprs_pp input
;;
