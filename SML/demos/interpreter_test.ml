(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open SML_lib.Interpreter

let _ =
  let prog = Stdio.In_channel.input_all Caml.stdin in
  Format.printf "%a_______\n" eval_pp prog
;;
