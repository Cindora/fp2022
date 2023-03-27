(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec pretty_print_parser exprs =
  let rec print_command command_string =
    let print_substr = function
      | STR s -> Printf.printf "%s" s
      | VAR v ->
        (match v with
         | "@" | "$" | "^" -> Printf.printf "$%s" v
         | _ -> Printf.printf "$(%s)" v)
    in
    match command_string with
    | substring :: substrings ->
      print_substr substring;
      print_command substrings
    | [] -> Printf.printf "\n"
  in
  let rec print_commands commands =
    match commands with
    | command :: commands ->
      Printf.printf "\t";
      print_command command;
      print_commands commands
    | [] -> Printf.printf "\n"
  in
  let choice_print = function
    | RULE rule ->
      Printf.printf "%s:%s\n" rule.targets rule.prerequisites;
      print_commands rule.commands
    | VAR_DEC (name, value) -> Printf.printf "%s=%s\n" name value
  in
  match exprs with
  | expr :: exprs ->
    choice_print expr;
    pretty_print_parser exprs
  | [] -> ()
;;

let pretty_print input =
  match Parser.parser input with
  | Result.Ok res -> pretty_print_parser res
  | Error _ -> ()
;;
