(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_number = function
  | '0' .. '9' -> true
  | _ -> false
;;

let skip_spaces =
  take_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let parens p = skip_spaces *> char '(' *> skip_spaces *> p <* skip_spaces <* char ')'
let integer = take_while1 is_number >>| int_of_string

(* sender for parsers *)
type sender =
  { snd_unary : sender -> Ast.expr Angstrom.t
  ; snd_binary : sender -> Ast.expr Angstrom.t
  ; snd_tuplexpr : sender -> Ast.expr Angstrom.t
  ; snd_list : sender -> Ast.expr Angstrom.t
  ; snd_cons_list : sender -> Ast.expr Angstrom.t
  ; snd_caseexpr : sender -> Ast.expr Angstrom.t
  ; snd_let : sender -> Ast.expr Angstrom.t
  ; snd_app : sender -> Ast.expr Angstrom.t
  ; snd_val_dec : sender -> Ast.expr Angstrom.t
  ; snd_val_rec_dec : sender -> Ast.expr Angstrom.t
  ; snd_arrfun : sender -> Ast.expr Angstrom.t
  ; snd_if_then_else : sender -> Ast.expr Angstrom.t
  ; snd_expr : sender -> Ast.expr Angstrom.t
  }

(* expression parsers *)
let snd_liter =
  fix (fun slf ->
    skip_spaces
    *>
    let int_snd_liter = integer >>| fun x -> BaseInt x in
    let char_snd_liter = char '\'' *> any_char <* char '\'' >>| fun x -> BaseChar x in
    let string_snd_liter =
      char '"' *> take_while (fun x -> x != '"') <* char '"' >>| fun x -> BaseString x
    in
    let bool_snd_liter =
      string "true" <|> string "false" >>| bool_of_string >>| fun x -> BaseBool x
    in
    let unit_snd_liter = string "()" >>| fun _ -> BaseUnit in
    let parser_literal =
      choice
        [ int_snd_liter
        ; char_snd_liter
        ; string_snd_liter
        ; bool_snd_liter
        ; unit_snd_liter
        ]
    in
    parens slf <|> lift expr_b_expr parser_literal)
;;

let snd_identifier =
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
    let parser_identifier =
      peek_char_fail
      >>= (fun first ->
            if (function
                | 'a' .. 'z' | '_' -> true
                | _ -> false)
                 first
            then
              take_while (function
                | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
                | _ -> false)
            else fail "Некорректный символ в имени.")
      >>= fun name ->
      if List.exists (fun x -> x = name) keywords
      then fail "Использовано ключевое слово в имени."
      else return (expr_identifier name)
    in
    parser_identifier)
;;

let snd_unary s =
  fix (fun slf ->
    skip_spaces
    *>
    let helper_parser_neg =
      choice
        [ parens (s.snd_unary s)
        ; snd_liter
        ; snd_identifier
        ; s.snd_binary s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_if_then_else s
        ]
    in
    let helper_parser_not =
      choice
        [ parens (s.snd_unary s)
        ; parens (s.snd_binary s)
        ; parens (s.snd_caseexpr s)
        ; parens (s.snd_let s)
        ; parens (s.snd_app s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> lift2 expr_unary_op (char '~' >>| u_neg) helper_parser_neg
    <|> lift2 expr_unary_op (string "not" >>| u_not) helper_parser_not)
;;

let snd_binary s =
  fix (fun _ ->
    skip_spaces
    *>
    let multdiv = skip_spaces *> choice [ char '*' >>| a_mult; char '/' >>| a_div ]
    and addsub = skip_spaces *> choice [ char '+' >>| a_add; char '-' >>| a_sub ]
    and relat =
      skip_spaces
      *> choice
           [ string ">=" >>| a_grteq
           ; string "<=" >>| a_lesseq
           ; char '>' >>| a_grt
           ; char '<' >>| a_less
           ]
    and equality = skip_spaces *> choice [ string "=" >>| a_eq; string "<>" >>| a_neq ]
    and andalso = skip_spaces *> (string "andalso" >>| a_and)
    and orelse = skip_spaces *> (string "orelse" >>| a_or) in
    let helper_parser =
      choice
        [ parens (s.snd_binary s)
        ; s.snd_unary s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    let rec parser_bin_op snd_exprarser op_parsers =
      let chainl1 snd_expr op_p =
        let rec go acc =
          lift2 (fun f x -> expr_binary_op f acc x) op_p snd_expr >>= go <|> return acc
        in
        snd_expr >>= fun init -> go init
      in
      match op_parsers with
      | [ op ] -> chainl1 snd_exprarser op
      | head :: tail -> chainl1 (parser_bin_op snd_exprarser tail) head
      | _ -> fail "Не является списком."
    in
    parser_bin_op helper_parser [ orelse; andalso; equality; relat; addsub; multdiv ])
;;

let parser_collection_helper brackets_parser constructor s =
  fix (fun slf ->
    skip_spaces
    *>
    let separator = skip_spaces *> char ',' *> skip_spaces <|> skip_spaces in
    let helper_parser =
      choice
        [ s.snd_tuplexpr s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf <|> lift constructor (brackets_parser (many (helper_parser <* separator))))
;;

let snd_tuplexpr s =
  let brackets parser = skip_spaces *> char '(' *> parser <* char ')' in
  parser_collection_helper brackets expr_tuple s
;;

let snd_list s =
  let brackets parser = skip_spaces *> char '[' *> parser <* char ']' in
  parser_collection_helper brackets expr_list s
;;

let snd_cons_list s =
  fix (fun slf ->
    skip_spaces
    *>
    let separator = skip_spaces *> string "::" *> skip_spaces
    and helper_parser =
      choice
        [ parens (s.snd_cons_list s)
        ; s.snd_unary s
        ; parens (s.snd_binary s)
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; parens (s.snd_caseexpr s)
        ; s.snd_let s
        ; parens (s.snd_app s)
        ; parens (s.snd_arrfun s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> lift2 expr_cons_list (helper_parser <* separator) (slf <|> helper_parser))
;;

let snd_caseexpr s =
  fix (fun slf ->
    skip_spaces
    *>
    let helper_parser_left =
      choice
        [ s.snd_unary s
        ; s.snd_cons_list s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; snd_liter
        ; snd_identifier
        ]
    in
    let helper_parser_right =
      choice
        [ s.snd_caseexpr s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_cons_list s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> string "case"
        *> lift2
             expr_casexpr_of
             helper_parser_right
             (let parser_case =
                lift2
                  (fun case action -> case, action)
                  helper_parser_left
                  (skip_spaces *> string "=>" *> helper_parser_right)
              and separator = skip_spaces *> string "|" in
              skip_spaces
              *> string "of"
              *> skip_spaces
              *> (string "|" <|> skip_spaces)
              *> sep_by1 separator parser_case))
;;

let snd_let s =
  fix (fun slf ->
    skip_spaces
    *>
    let helper_parser =
      choice
        [ s.snd_let s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> string "let"
        *> take_while1 (function
             | ' ' | '\t' | '\n' | '\r' -> true
             | _ -> false)
        *> lift2
             expr_let_in
             (many1 (s.snd_val_dec s <|> s.snd_val_rec_dec s))
             (skip_spaces *> string "in" *> helper_parser <* skip_spaces <* string "end"))
;;

let snd_app s =
  fix (fun slf ->
    skip_spaces
    *>
    let parser_func =
      choice
        [ parens (s.snd_caseexpr s)
        ; parens (s.snd_let s)
        ; parens (s.snd_arrfun s)
        ; parens (s.snd_if_then_else s)
        ; snd_identifier
        ]
    in
    let parser_op =
      choice
        [ parens (s.snd_app s)
        ; parens (s.snd_unary s)
        ; parens (s.snd_binary s)
        ; parens (s.snd_tuplexpr s)
        ; s.snd_list s
        ; parens (s.snd_cons_list s)
        ; parens (s.snd_caseexpr s)
        ; parens (s.snd_let s)
        ; parens (s.snd_arrfun s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    let apply_lift acc = lift (expr_application acc) parser_op in
    let rec go acc = apply_lift acc >>= go <|> return acc in
    parens slf <|> parser_func >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parser_value_declaration_helper keyword constructor s =
  fix (fun _ ->
    skip_spaces
    *> string keyword
    *> skip_spaces
    *>
    let helper_parser =
      choice
        [ s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    lift2
      constructor
      (snd_identifier
       >>= (function
             | XIdentifier x -> return x
             | _ -> fail "Переменная недоступна.")
       >>= fun name ->
       match name with
       | "_" -> fail "Подстановка не ожидается."
       | _ -> return name)
      (skip_spaces *> string "=" *> helper_parser))
;;

let snd_val_dec s = parser_value_declaration_helper "val" expr_val_dec s
let snd_val_rec_dec s = parser_value_declaration_helper "val rec" expr_val_rec_dec s

let snd_arrfun s =
  fix (fun slf ->
    skip_spaces
    *>
    let helper_parser =
      choice
        [ s.snd_arrfun s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> string "fn"
        *> lift2
             expr_arrow_fun
             (snd_identifier
              >>= (function
                    | XIdentifier x -> return [ x ]
                    | _ -> fail "Переменная недоступна.")
              <* skip_spaces
              <* string "=>"
              <* skip_spaces)
             (helper_parser <* skip_spaces))
;;

let snd_if_then_else s =
  fix (fun slf ->
    skip_spaces
    *>
    let helper_parser =
      choice
        [ s.snd_if_then_else s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuplexpr s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_caseexpr s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> string "if"
        *> lift3
             expr_if_then_else
             helper_parser
             (skip_spaces *> string "then" *> helper_parser)
             (skip_spaces *> string "else" *> helper_parser))
;;

let snd_expr s =
  choice
    [ s.snd_unary s
    ; s.snd_binary s
    ; s.snd_tuplexpr s
    ; s.snd_list s
    ; s.snd_cons_list s
    ; s.snd_caseexpr s
    ; s.snd_let s
    ; s.snd_app s
    ; s.snd_val_dec s
    ; s.snd_val_rec_dec s
    ; s.snd_arrfun s
    ; s.snd_if_then_else s
    ; snd_liter
    ; snd_identifier
    ]
;;

let snd_default =
  { snd_unary
  ; snd_binary
  ; snd_tuplexpr
  ; snd_list
  ; snd_cons_list
  ; snd_caseexpr
  ; snd_let
  ; snd_app
  ; snd_val_dec
  ; snd_val_rec_dec
  ; snd_arrfun
  ; snd_if_then_else
  ; snd_expr
  }
;;

let parse_strings str = parse_string ~consume:Prefix (snd_expr snd_default) str
