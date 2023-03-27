(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Make_lib

let () =
  (*Stdlib.Sys.chdir "demo_project";*)
  let input = Core.In_channel.read_all "./Makefile" in
  Interpret.interpret input (Interpret.args_list Sys.argv)
;;
