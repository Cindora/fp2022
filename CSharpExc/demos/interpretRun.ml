(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open CSharpExc_lib.Parser.Fields_and_classes
open CSharpExc_lib.Parser
open CSharpExc_lib.Types_setup
open CSharpExc_lib.Interpreter.Interpreter (CSharpExc_lib.Interpreter.Result)

let interpretation class_list_ast class_map =
  match interpret_cl class_list_ast class_map with
  | Error message -> print_endline message
  | Ok load_map ->
    (match start_interpret load_map with
     | Error message -> print_endline message
     | Ok _ -> print_endline "")
;;

let interpret s =
  let parse_s = Option.get (parsing parser s) in
  interpretation parse_s KeyMap.empty
;;

let () = interpret (Stdio.In_channel.input_all stdin)
