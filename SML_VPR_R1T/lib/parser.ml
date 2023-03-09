(** Copyright 2022-2023, Anton Kraev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* helper functions and parsers *)
let is_empty = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_char = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_varname = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let skip_spaces = take_while is_empty
let parens p = skip_spaces *> char '(' *> skip_spaces *> p <* skip_spaces <* char ')'

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let varname =
  peek_char_fail
  >>= fun first ->
  if is_char first
  then take_while is_varname
  else fail "Parsing error: bad first symbol of id."
;;

let id_of_expr = function
  | EIdentifier x -> return x
  | _ -> fail "Unreachable"
;;

let id_list_of_expr = function
  | EIdentifier x -> return [ x ]
  | _ -> fail "Unreachable"
;;

(* dispatch for parsers *)
type dispatch =
  { d_binarop : dispatch -> Ast.expr Angstrom.t
  ; d_unarop : dispatch -> Ast.expr Angstrom.t
  ; d_tuple : dispatch -> Ast.expr Angstrom.t
  ; d_list : dispatch -> Ast.expr Angstrom.t
  ; d_cons_list : dispatch -> Ast.expr Angstrom.t
  ; d_expr : dispatch -> Ast.expr Angstrom.t
  }

(* Expressions parsers *)
let b_d_expr =
  fix (fun self ->
    skip_spaces
    *>
    let int_b_d_expr = integer >>| fun x -> BaseInt x in
    let char_b_d_expr = char '\'' *> any_char <* char '\'' >>| fun x -> BaseChar x in
    let string_b_d_expr =
      char '"' *> take_while (fun x -> x != '"') <* char '"' >>| fun x -> BaseString x
    in
    let bool_b_d_expr =
      string "true" <|> string "false" >>| bool_of_string >>| fun x -> BaseBool x
    in
    let unit_b_d_expr = string "()" >>| fun _ -> BaseUnit in
    let parse_b_expr =
      choice
        [ int_b_d_expr; char_b_d_expr; string_b_d_expr; bool_b_d_expr; unit_b_d_expr ]
    in
    parens self <|> lift e_b_expr parse_b_expr)
;;

let identifier_p =
  fix (fun _ ->
    skip_spaces
    *>
    let keywords =
      [ "let"
      ; "fun"
      ; "val"
      ; "rec"
      ; "case"
      ; "of"
      ; "if"
      ; "then"
      ; "else"
      ; "in"
      ; "fn"
      ; "end"
      ; "true"
      ; "false"
      ; "not"
      ; "orelse"
      ; "andalso"
      ]
    in
    let parse_identifier =
      varname
      >>= fun name ->
      if List.exists (fun x -> x = name) keywords
      then fail "Parsing error: keyword used."
      else return (e_identifier name)
    in
    parse_identifier)
;;

let d_unarop d =
  fix (fun self ->
    skip_spaces
    *>
    let parse_content_neg =
      choice [ parens self; b_d_expr; identifier_p; d.d_binarop d ]
    in
    let parse_content_not =
      choice [ parens (d.d_unarop d); parens (d.d_binarop d); b_d_expr; identifier_p ]
    in
    parens self
    <|> lift2 e_unary_op (char '~' >>| uneg) parse_content_neg
    <|> lift2 e_unary_op (string "not" >>| unot) parse_content_not)
;;

let d_binarop d =
  fix (fun self ->
    skip_spaces
    *>
    let muldiv = skip_spaces *> choice [ char '*' >>| a_mul; char '/' >>| a_div ]
    and addsub = skip_spaces *> choice [ char '+' >>| a_add; char '-' >>| a_sub ]
    and relat =
      skip_spaces
      *> choice
           [ string ">=" >>| a_gte
           ; string "<=" >>| a_lse
           ; char '>' >>| a_gt
           ; char '<' >>| a_ls
           ]
    and equal = skip_spaces *> choice [ string "=" >>| a_eq; string "<>" >>| a_neq ]
    and andalso = skip_spaces *> (string "andalso" >>| a_and)
    and orelse = skip_spaces *> (string "orelse" >>| a_or) in
    let parse_content =
      choice
        [ parens self; d.d_unarop d; d.d_list d; d.d_cons_list d; b_d_expr; identifier_p ]
    in
    let rec parse_bin_op d_exprarser op_parsers =
      let chainl1 d_expr op_p =
        let rec go acc =
          lift2 (fun f x -> e_binary_op f acc x) op_p d_expr >>= go <|> return acc
        in
        d_expr >>= fun init -> go init
      in
      match op_parsers with
      | [ op ] -> chainl1 d_exprarser op
      | h :: t -> chainl1 (parse_bin_op d_exprarser t) h
      | _ -> fail "Unreachable"
    in
    parse_bin_op parse_content [ orelse; andalso; equal; relat; addsub; muldiv ])
;;

let parse_collection_helper brackets_parser constructor d =
  fix (fun self ->
    skip_spaces
    *>
    let separator = skip_spaces *> char ',' *> skip_spaces <|> skip_spaces in
    let parse_content =
      choice
        [ d.d_tuple d
        ; d.d_unarop d
        ; d.d_binarop d
        ; d.d_list d
        ; d.d_cons_list d
        ; b_d_expr
        ; identifier_p
        ]
    in
    parens self <|> lift constructor (brackets_parser (many (parse_content <* separator))))
;;

let d_tuple d =
  let brackets parser = skip_spaces *> char '(' *> parser <* char ')' in
  parse_collection_helper brackets e_tuple d
;;

let d_list d =
  let brackets parser = skip_spaces *> char '[' *> parser <* char ']' in
  parse_collection_helper brackets e_list d
;;

let d_cons_list d =
  fix (fun self ->
    skip_spaces
    *>
    let separator = skip_spaces *> string "::" *> skip_spaces
    and parse_content =
      choice
        [ parens (d.d_cons_list d)
        ; d.d_unarop d
        ; parens (d.d_binarop d)
        ; d.d_tuple d
        ; d.d_list d
        ; b_d_expr
        ; identifier_p
        ]
    in
    parens self <|> lift2 e_cons_list (parse_content <* separator) (self <|> parse_content))
;;

(* Parser of general expression *)
let d_expr d =
  choice
    [ d.d_unarop d
    ; d.d_binarop d
    ; d.d_tuple d
    ; d.d_list d
    ; d.d_cons_list d
    ; b_d_expr
    ; identifier_p
    ]
;;

(* Default expression dispatch *)
let default_d = { d_unarop; d_binarop; d_tuple; d_list; d_cons_list; d_expr }

(* main parser *)
let parse str = parse_string ~consume:Prefix (d_expr default_d) str
