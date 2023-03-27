(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_whitespace = function
  | ' ' -> true
  | _ -> false
;;

let is_tab = function
  | '\t' -> true
  | _ -> false
;;

let is_newline = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let is_empty_char c = is_tab c || is_whitespace c || is_newline c

let is_valid_var_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let not_newline = Fun.negate is_newline

let not_hash = function
  | '#' -> false
  | _ -> true
;;

let not_close_bracket = function
  | ')' -> false
  | _ -> true
;;

let not_dollar = function
  | '$' -> false
  | _ -> true
;;

let not_backslash = function
  | '\\' -> false
  | _ -> true
;;

let not_colon = function
  | ':' -> false
  | _ -> true
;;

let all_pred preds el = List.for_all (fun f -> f el) preds
let some_pred preds el = List.exists (fun f -> f el) preds

let multy_string =
  many
    (take_while (all_pred [ not_newline; not_backslash; not_colon; not_hash ])
    <* char '\\'
    <* char '\n')
;;

let simple_string pre sep =
  pre *> take_while (all_pred [ not_newline; not_backslash; not_colon; not_hash ]) <* sep
;;

let simple_string1 pre sep =
  pre *> take_while1 (all_pred [ not_newline; not_backslash; not_colon; not_hash ]) <* sep
;;

(* Comment *)
let comment_string =
  let multy_string =
    string "\\n" *> take_while (all_pred [ not_backslash; not_newline; not_hash ])
  in
  let start_string =
    char '#' *> take_while1 (all_pred [ not_backslash; not_newline; not_hash ])
  in
  let compare _ _ _ = [ () ] in
  lift3 compare start_string (many multy_string) (many1 end_of_line)
;;

let empty_string = take_while1 is_empty_char *> many1 end_of_line <|> many1 end_of_line
let meaningless_string = many (comment_string <|> empty_string)

exception InvalidVarName of string
exception InvalidString of string * string

let words_declaration =
  many
    (skip_while (some_pred [ is_empty_char ])
    *> take_while1
         (all_pred [ not_backslash; not_colon; not_hash; Fun.negate is_empty_char ]))
  <* skip_while (some_pred [ is_empty_char ])
;;

let words_parser input =
  match parse_string ~consume:Consume.All words_declaration input with
  | Result.Ok res -> res
  | Error e -> raise (InvalidString (input, e))
;;

let substrings_declaration =
  let str_constructor el = STR el in
  let var_constructor el = VAR el in
  let concat_list_str_list a b c = List.concat [ a; [ str_constructor b ]; c ] in
  let var_dec =
    peek_char_fail
    >>= function
    | '(' -> take 1 *> (take_while1 is_valid_var_char >>| var_constructor) <* char ')'
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '@' | '^' | '$' ->
      take 1 >>| var_constructor
    | c -> raise (InvalidVarName (Char.escaped c))
  in
  let var_declaration = char '$' *> var_dec in
  let str_declaration = take_while1 not_dollar in
  lift
    List.concat
    (many
       (lift3
          concat_list_str_list
          (many var_declaration)
          str_declaration
          (many var_declaration)))
;;

let substrings_parser input =
  match parse_string ~consume:Consume.All substrings_declaration input with
  | Result.Ok res -> res
  | Error e -> raise (InvalidString (input, e))
;;

(* Targets *)
let targets_declaration =
  let concat_str_list a b = String.concat " " (List.concat [ a; [ b ] ]) in
  lift2 concat_str_list multy_string (simple_string1 (string "") (char ':'))
;;

(* Prerequisites *)
let prerequisites_declaration =
  let concat_str_list a b = String.concat " " (List.concat [ a; [ b ] ]) in
  lift2 concat_str_list multy_string (simple_string (string "") (many end_of_line))
;;

(* Command *)
let command_string =
  let add_end_str c a = List.append a [ STR c ] in
  let concat_list a b _ =
    let a =
      match b with
      | [] -> add_end_str "" a
      | _ -> add_end_str "\\\n" a
    in
    let b =
      List.mapi (fun i -> add_end_str (if List.length b = i then "\\\n" else "")) b
    in
    List.append a (List.concat b)
  in
  let multy_string =
    char '\\'
    *> char '\n'
    *> take_while (all_pred [ not_backslash; not_newline; not_hash ])
  in
  let start_string =
    char '\t' *> take_while1 (all_pred [ not_backslash; not_newline; not_hash ])
  in
  lift3
    concat_list
    (start_string >>| substrings_parser)
    (many (multy_string >>| substrings_parser))
    (many end_of_line)
;;

let commands_declaration = many command_string

(* Var *)
let var_declaration =
  let var_name = take_while1 is_valid_var_char in
  let name = var_name <* skip_while (fun c -> is_tab c || is_whitespace c) <* char '=' in
  let value = take_while not_newline <* many end_of_line in
  let var_constructor name value = VAR_DEC (name, value) in
  lift2 var_constructor name value
;;

let exprs =
  let rule_constructor t p c = RULE { targets = t; prerequisites = p; commands = c } in
  let rule_declaration =
    lift3
      rule_constructor
      targets_declaration
      prerequisites_declaration
      commands_declaration
    <* many end_of_line
  in
  many (rule_declaration <|> var_declaration)
;;

let parser str = parse_string ~consume:Consume.All exprs str

(*=============================*)
(*============TESTS============*)
(*=============================*)

let test_ok parser input expected =
  match parse_string ~consume:Consume.All parser input with
  | Ok res when res = expected -> true
  | Ok _ ->
    Printf.printf "%s\n" input;
    false
  | Error e ->
    Printf.printf "%s\n" e;
    false
;;

let test_fail parser input =
  match parse_string ~consume:Consume.All parser input with
  | Ok _ ->
    Printf.printf "%s\n" input;
    false
  | Error _ -> true
;;

(*============================*)
(*== Test words_declaration ==*)
let parse_ok = test_ok words_declaration
let parse_fail = test_fail words_declaration

let%test _ = parse_ok "ab" [ "ab" ]
let%test _ = parse_ok "ab bb" [ "ab"; "bb" ]
let%test _ = parse_ok "" []
let%test _ = parse_ok " " []
let%test _ = parse_ok " a" [ "a" ]
let%test _ = parse_ok "a " [ "a" ]
let%test _ = parse_ok " a " [ "a" ]
let%test _ = parse_ok " a\n" [ "a" ]
let%test _ = parse_fail "sdf#"
let%test _ = parse_fail "sdf\\"

(*=============================*)
(*== Test substrings_declaration ==*)
let parse_ok = test_ok substrings_declaration
let parse_fail = test_fail substrings_declaration

let%test _ = parse_ok "$(a) " [ VAR "a"; STR " " ]
let%test _ = parse_ok "$a " [ VAR "a"; STR " " ]
let%test _ = parse_ok " $(a)" [ STR " "; VAR "a" ]
let%test _ = parse_ok " b " [ STR " b " ]
let%test _ = parse_ok " \\n " [ STR " \\n " ]
let%test _ = parse_ok "" []
let%test _ = parse_fail "$(a)" (* MUST BE FIXED *)
let%test _ = parse_fail "bb$"

(*==============================*)
(*== Test targets_declaration ==*)
let parse_ok = test_ok targets_declaration
let parse_fail = test_fail targets_declaration

(* Simple string *)
let%test _ = parse_ok "a:" "a"
let%test _ = parse_ok "a b:" "a b"
let%test _ = parse_fail "a b"
let%test _ = parse_fail ":"
let%test _ = parse_fail ""
(* Multy string *)
let%test _ = parse_ok "a\\\nb:" "a b"
let%test _ = parse_ok "a\\\nb\\\nc:" "a b c"
let%test _ = parse_ok "a\\\n\tb:" "a \tb"
let%test _ = parse_ok "a\\\n\\\nc:" "a  c"

(*====================================*)
(*== Test prerequisites_declaration ==*)
let parse_ok = test_ok prerequisites_declaration
let parse_fail = test_fail prerequisites_declaration

(* Simple string *)
let%test _ = parse_ok "a\n" "a"
let%test _ = parse_ok "a b\n" "a b"
let%test _ = parse_ok "a b" "a b"
let%test _ = parse_ok "\n" ""
let%test _ = parse_ok "" ""
(* Multy string *)
let%test _ = parse_ok "a\\\nb\n" "a b"
let%test _ = parse_ok "a\\\nb\\\nc\n" "a b c"
let%test _ = parse_ok "a\\\n\\\nc\n" "a  c"

(*===============================*)
(*== Test commands_declaration ==*)
(*  WHEN LIST.CONCAT ADD  "\\n"  *)
let parse_ok = test_ok commands_declaration
let parse_fail = test_fail commands_declaration

(* Simple string *)
let%test _ = parse_ok "\ta" [ [ STR "a"; STR "" ] ]
let%test _ = parse_ok "\ta\n\n\n" [ [ STR "a"; STR "" ] ]
let%test _ = parse_ok "\t$a " [ [ VAR "a"; STR " "; STR "" ] ]
let%test _ = parse_ok "\ta$(b)" [ [ STR "a"; VAR "b"; STR "" ] ]
let%test _ = parse_ok "\ta\n" [ [ STR "a"; STR "" ] ]
let%test _ = parse_ok "\ta\n\ta" [ [ STR "a"; STR "" ]; [ STR "a"; STR "" ] ]
let%test _ = parse_ok "\ta\n\n\ta" [ [ STR "a"; STR "" ]; [ STR "a"; STR "" ] ]
let%test _ = parse_fail "\ta\na"
let%test _ = parse_fail "a"
let%test _ = parse_fail "\t\n"
let%test _ = parse_fail "\t\\n"
(* Multy string *)
let%test _ = parse_ok "\tab\\\nb" [ [ STR "ab"; STR "\\\n"; STR "b"; STR "" ] ]
let%test _ = parse_ok "\tab\\\n\tb" [ [ STR "ab"; STR "\\\n"; STR "\tb"; STR "" ] ]
let%test _ = parse_ok "\tab\\\n\tb\n" [ [ STR "ab"; STR "\\\n"; STR "\tb"; STR "" ] ]

let%test _ =
  parse_ok
    "\tab\\\n\tb\n\ta"
    [ [ STR "ab"; STR "\\\n"; STR "\tb"; STR "" ]; [ STR "a"; STR "" ] ]
;;

let%test _ =
  parse_ok
    "\ta\\\nb\n\n\ta"
    [ [ STR "a"; STR "\\\n"; STR "b"; STR "" ]; [ STR "a"; STR "" ] ]
;;

(*==========================*)
(*== Test var_declaration ==*)
let parse_ok = test_ok var_declaration
let parse_fail = test_fail var_declaration

let%test _ = parse_ok "a=10" (VAR_DEC ("a", "10"))
let%test _ = parse_ok "a =10" (VAR_DEC ("a", "10"))
let%test _ = parse_ok "a = 10 " (VAR_DEC ("a", " 10 "))
let%test _ = parse_ok "a =" (VAR_DEC ("a", ""))
let%test _ = parse_ok "a =\n\n\n\n" (VAR_DEC ("a", ""))
let%test _ = parse_fail "a: 0"
let%test _ = parse_fail "a"
let%test _ = parse_fail "$=10"
let%test _ = parse_fail ";=10"

(*=========================*)
(*== Test comment_string ==*)

(*=============================*)
(*== Test meaningless_string ==*)

(*=================*)
(*== Test Parser ==*)

let parse_ok = test_ok exprs

let%test _ =
  parse_ok
    "a\\\nb : s\\\ns2\n\techo a\\\n\tb"
    [ RULE
        { targets = "a b "
        ; prerequisites = " s s2"
        ; commands = [ [ STR "echo a"; STR "\\\n"; STR "\tb"; STR "" ] ]
        }
    ]
;;

let%test _ =
  parse_ok
    "a\\\nb : s\\\ns2\n\techo a\\\nb"
    [ RULE
        { targets = "a b "
        ; prerequisites = " s s2"
        ; commands = [ [ STR "echo a"; STR "\\\n"; STR "b"; STR "" ] ]
        }
    ]
;;
