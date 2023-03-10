(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* helper functions and parsers *)
let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_first_char = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_varname_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let skip_spaces = take_while is_space
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
  if is_first_char first
  then take_while is_varname_char
  else fail "Parsing error: bad first symbol of id."
;;

let id_of_expr = function
  | XIdentifier x -> return x
  | _ -> fail "Unreachable"
;;

let id_list_of_expr = function
  | XIdentifier x -> return [ x ]
  | _ -> fail "Unreachable"
;;

(* dispatch for parsers *)
type dispatch =
  { unary_op_p : dispatch -> Ast.expr Angstrom.t
  ; binary_op_p : dispatch -> Ast.expr Angstrom.t
  ; tuple_p : dispatch -> Ast.expr Angstrom.t
  ; list_p : dispatch -> Ast.expr Angstrom.t
  ; cons_list_p : dispatch -> Ast.expr Angstrom.t
  ; case_of_p : dispatch -> Ast.expr Angstrom.t
  ; let_in_p : dispatch -> Ast.expr Angstrom.t
  ; application_p : dispatch -> Ast.expr Angstrom.t
  ; val_dec_p : dispatch -> Ast.expr Angstrom.t
  ; val_rec_dec_p : dispatch -> Ast.expr Angstrom.t
  ; arrow_fun_p : dispatch -> Ast.expr Angstrom.t
  ; if_then_else_p : dispatch -> Ast.expr Angstrom.t
  ; expr_p : dispatch -> Ast.expr Angstrom.t
  }

(* expression parsers *)
let literal_p =
  fix (fun slf ->
    skip_spaces
    *>
    let int_literal_p = integer >>| fun x -> BaseInt x in
    let char_literal_p = char '\'' *> any_char <* char '\'' >>| fun x -> BaseChar x in
    let string_literal_p =
      char '"' *> take_while (fun x -> x != '"') <* char '"' >>| fun x -> BaseString x
    in
    let bool_literal_p =
      string "true" <|> string "false" >>| bool_of_string >>| fun x -> BaseBool x
    in
    let unit_literal_p = string "()" >>| fun _ -> BaseUnit in
    let parse_literal =
      choice
        [ int_literal_p
        ; char_literal_p
        ; string_literal_p
        ; bool_literal_p
        ; unit_literal_p
        ]
    in
    parens slf <|> lift e_b_expr parse_literal)
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

let unary_op_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let parse_content_neg =
      choice
        [ parens (d.unary_op_p d)
        ; literal_p
        ; identifier_p
        ; d.binary_op_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.if_then_else_p d
        ]
    in
    let parse_content_not =
      choice
        [ parens (d.unary_op_p d)
        ; parens (d.binary_op_p d)
        ; parens (d.case_of_p d)
        ; parens (d.let_in_p d)
        ; parens (d.application_p d)
        ; parens (d.if_then_else_p d)
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf
    <|> lift2 e_unary_op (char '~' >>| uneg) parse_content_neg
    <|> lift2 e_unary_op (string "not" >>| unot) parse_content_not)
;;

let binary_op_p d =
  fix (fun _ ->
    skip_spaces
    *>
    let multdiv = skip_spaces *> choice [ char '*' >>| a_mul; char '/' >>| a_div ]
    and addsub = skip_spaces *> choice [ char '+' >>| a_add; char '-' >>| a_sub ]
    and relat =
      skip_spaces
      *> choice
           [ string ">=" >>| a_gte
           ; string "<=" >>| a_lse
           ; char '>' >>| a_gt
           ; char '<' >>| a_ls
           ]
    and equality = skip_spaces *> choice [ string "=" >>| a_eq; string "<>" >>| a_neq ]
    and andalso = skip_spaces *> (string "andalso" >>| a_and)
    and orelse = skip_spaces *> (string "orelse" >>| a_or) in
    let parse_content =
      choice
        [ parens (d.binary_op_p d)
        ; d.unary_op_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    let rec parse_bin_op expr_parser op_parsers =
      let chainl1 expr_p op_p =
        let rec go acc =
          lift2 (fun f x -> e_binary_op f acc x) op_p expr_p >>= go <|> return acc
        in
        expr_p >>= fun init -> go init
      in
      match op_parsers with
      | [ op ] -> chainl1 expr_parser op
      | h :: t -> chainl1 (parse_bin_op expr_parser t) h
      | _ -> fail "Fail: Not a list"
    in
    parse_bin_op parse_content [ orelse; andalso; equality; relat; addsub; multdiv ])
;;

let parse_collection_helper brackets_parser constructor d =
  fix (fun slf ->
    skip_spaces
    *>
    let separator = skip_spaces *> char ',' *> skip_spaces <|> skip_spaces in
    let parse_content =
      choice
        [ d.tuple_p d
        ; d.unary_op_p d
        ; d.binary_op_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf <|> lift constructor @@ brackets_parser @@ many (parse_content <* separator))
;;

let tuple_p d =
  let brackets parser = skip_spaces *> char '(' *> parser <* char ')' in
  parse_collection_helper brackets e_tuple d
;;

let list_p d =
  let brackets parser = skip_spaces *> char '[' *> parser <* char ']' in
  parse_collection_helper brackets e_list d
;;

let cons_list_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let separator = skip_spaces *> string "::" *> skip_spaces
    and parse_content =
      choice
        [ parens (d.cons_list_p d)
        ; d.unary_op_p d
        ; parens @@ d.binary_op_p d
        ; d.tuple_p d
        ; d.list_p d
        ; parens @@ d.case_of_p d
        ; d.let_in_p d
        ; parens @@ d.application_p d
        ; parens @@ d.arrow_fun_p d
        ; parens @@ d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf <|> lift2 e_cons_list (parse_content <* separator) (slf <|> parse_content))
;;

let case_of_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let parse_content_left =
      choice
        [ d.unary_op_p d
        ; d.cons_list_p d
        ; d.tuple_p d
        ; d.list_p d
        ; literal_p
        ; identifier_p
        ]
    in
    let parse_content_right =
      choice
        [ d.case_of_p d
        ; d.unary_op_p d
        ; d.binary_op_p d
        ; d.cons_list_p d
        ; d.tuple_p d
        ; d.list_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.arrow_fun_p d
        ; d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf
    <|> string "case"
        *> lift2
             e_case_of
             parse_content_right
             (let parse_case =
                lift2
                  (fun case action -> case, action)
                  parse_content_left
                  (skip_spaces *> string "=>" *> parse_content_right)
              and separator = skip_spaces *> string "|" in
              skip_spaces
              *> string "of"
              *> skip_spaces
              *> (string "|" <|> skip_spaces)
              *> sep_by1 separator parse_case))
;;

let let_in_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let parse_content =
      choice
        [ d.let_in_p d
        ; d.unary_op_p d
        ; d.binary_op_p d
        ; d.tuple_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.application_p d
        ; d.arrow_fun_p d
        ; d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf
    <|> string "let"
        *> take_while1 is_space
        *> lift2
             e_let_in
             (many1 (d.val_dec_p d <|> d.val_rec_dec_p d))
             (skip_spaces *> string "in" *> parse_content <* skip_spaces <* string "end"))
;;

let application_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let function_parser =
      choice
        [ parens (d.case_of_p d)
        ; parens (d.let_in_p d)
        ; parens (d.arrow_fun_p d)
        ; parens (d.if_then_else_p d)
        ; identifier_p
        ]
    in
    let operand_parser =
      choice
        [ parens (d.application_p d)
        ; parens (d.unary_op_p d)
        ; parens (d.binary_op_p d)
        ; parens (d.tuple_p d)
        ; d.list_p d
        ; parens (d.cons_list_p d)
        ; parens (d.case_of_p d)
        ; parens (d.let_in_p d)
        ; parens (d.arrow_fun_p d)
        ; parens (d.if_then_else_p d)
        ; literal_p
        ; identifier_p
        ]
    in
    let apply_lift acc = lift (e_application acc) operand_parser in
    let rec go acc = apply_lift acc >>= go <|> return acc in
    parens slf <|> function_parser >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parse_value_declaration_helper keyword constructor d =
  fix (fun _ ->
    skip_spaces
    *> string keyword
    *> skip_spaces
    *>
    let parse_content =
      choice
        [ d.unary_op_p d
        ; d.binary_op_p d
        ; d.tuple_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.arrow_fun_p d
        ; d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    lift2
      constructor
      (identifier_p
       >>= id_of_expr
       >>= fun name ->
       if name = "_" then fail "Parsing error: wildcard not expected." else return name)
      (skip_spaces *> string "=" *> parse_content))
;;

let val_dec_p d = parse_value_declaration_helper "val" e_val_dec d
let val_rec_dec_p d = parse_value_declaration_helper "val rec" e_val_rec_dec d

let arrow_fun_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let parse_content =
      choice
        [ d.arrow_fun_p d
        ; d.unary_op_p d
        ; d.binary_op_p d
        ; d.tuple_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.if_then_else_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf
    <|> string "fn"
        *> lift2
             e_arrow_fun
             (identifier_p
              >>= id_list_of_expr
              <* skip_spaces
              <* string "=>"
              <* skip_spaces)
             (parse_content <* skip_spaces))
;;

let if_then_else_p d =
  fix (fun slf ->
    skip_spaces
    *>
    let parse_content =
      choice
        [ d.if_then_else_p d
        ; d.unary_op_p d
        ; d.binary_op_p d
        ; d.tuple_p d
        ; d.list_p d
        ; d.cons_list_p d
        ; d.case_of_p d
        ; d.let_in_p d
        ; d.application_p d
        ; d.arrow_fun_p d
        ; literal_p
        ; identifier_p
        ]
    in
    parens slf
    <|> string "if"
        *> lift3
             e_if_then_else
             parse_content
             (skip_spaces *> string "then" *> parse_content)
             (skip_spaces *> string "else" *> parse_content))
;;

(* Parser of general expression *)
let expr_p d =
  choice
    [ d.unary_op_p d
    ; d.binary_op_p d
    ; d.tuple_p d
    ; d.list_p d
    ; d.cons_list_p d
    ; d.case_of_p d
    ; d.let_in_p d
    ; d.application_p d
    ; d.val_dec_p d
    ; d.val_rec_dec_p d
    ; d.arrow_fun_p d
    ; d.if_then_else_p d
    ; literal_p
    ; identifier_p
    ]
;;

(* Default expression dispatch *)
let default_d =
  { unary_op_p
  ; binary_op_p
  ; tuple_p
  ; list_p
  ; cons_list_p
  ; case_of_p
  ; let_in_p
  ; application_p
  ; val_dec_p
  ; val_rec_dec_p
  ; arrow_fun_p
  ; if_then_else_p
  ; expr_p
  }
;;

(* main parser *)
let parse_strings str = parse_string ~consume:Prefix (expr_p default_d) str
