(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_number = function
  | '0' .. '9' -> true
  | _ -> false
;;

let constr_int = take_while1 is_number >>| int_of_string

let skip_spaces =
  take_while (function
    | ' ' | '\n' | '\r' -> true
    | _ -> false)
;;

let skip_spaces_rstr str = skip_spaces *> string str
let skip_spaces_lstr str = skip_spaces <* string str
let skip_spaces_rchar chr = skip_spaces *> char chr
let skip_spaces_lchar chr = skip_spaces <* char chr
let parens s = skip_spaces_rchar '(' *> skip_spaces *> s <* skip_spaces_lchar ')'

type sender =
  { snd_unary : sender -> Ast.expr Angstrom.t
  ; snd_binary : sender -> Ast.expr Angstrom.t
  ; snd_tuple : sender -> Ast.expr Angstrom.t
  ; snd_list : sender -> Ast.expr Angstrom.t
  ; snd_cons_list : sender -> Ast.expr Angstrom.t
  ; snd_case : sender -> Ast.expr Angstrom.t
  ; snd_let : sender -> Ast.expr Angstrom.t
  ; snd_app : sender -> Ast.expr Angstrom.t
  ; snd_val_dec : sender -> Ast.expr Angstrom.t
  ; snd_val_rec_dec : sender -> Ast.expr Angstrom.t
  ; snd_arrfun : sender -> Ast.expr Angstrom.t
  ; snd_if_then_else : sender -> Ast.expr Angstrom.t
  ; snd_expr : sender -> Ast.expr Angstrom.t
  }

(** Expressions parsers *)
let snd_liter =
  let int_snd_liter = constr_int >>| fun x -> BaseInt x in
  let char_snd_liter = char '\'' *> any_char <* char '\'' >>| fun x -> BaseChar x in
  let string_snd_liter =
    char '"' *> take_while (fun x -> x != '"') <* char '"' >>| fun x -> BaseString x
  in
  let bool_snd_liter =
    string "true" <|> string "false" >>| bool_of_string >>| fun x -> BaseBool x
  in
  let unit_snd_liter = string "()" >>| fun _ -> BaseUnit in
  fix (fun slf ->
    skip_spaces
    *>
    let parser_literal =
      choice
        [ int_snd_liter
        ; char_snd_liter
        ; string_snd_liter
        ; bool_snd_liter
        ; unit_snd_liter
        ]
    in
    parens slf <|> lift (fun b -> XLiteral b) parser_literal)
;;

let snd_identifier =
  let keywords =
    [ "let"
    ; "in"
    ; "fn"
    ; "end"
    ; "fun"
    ; "rec"
    ; "val"
    ; "true"
    ; "false"
    ; "not"
    ; "orelse"
    ; "andalso"
    ; "case"
    ; "of"
    ; "if"
    ; "then"
    ; "else"
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
          else fail "Invalid name character.")
    >>= fun name ->
    if List.exists (fun x -> x = name) keywords
    then fail "Keyword cannot be used in the name."
    else return ((fun b -> XIdentifier b) name)
  in
  fix (fun _ -> skip_spaces *> parser_identifier)
;;

let snd_unary s =
  let constr_unary_op op b = XUnaryOp (op, b) in
  fix (fun slf ->
    skip_spaces
    *>
    let helper_neg =
      choice
        [ parens (s.snd_unary s)
        ; snd_liter
        ; snd_identifier
        ; s.snd_binary s
        ; s.snd_case s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_if_then_else s
        ]
    in
    let helper_not =
      choice
        [ parens (s.snd_unary s)
        ; parens (s.snd_binary s)
        ; parens (s.snd_case s)
        ; parens (s.snd_let s)
        ; parens (s.snd_app s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> lift2 constr_unary_op (char '~' >>| fun _ -> Neg) helper_neg
    <|> lift2 constr_unary_op (string "not" >>| fun _ -> Not) helper_not)
;;

let snd_binary s =
  let constr_binary_op op left_expr right_expr = XBinaryOp (op, left_expr, right_expr) in
  let multdiv =
    skip_spaces *> choice [ (char '*' >>| fun _ -> Mult); (char '/' >>| fun _ -> Div) ]
  in
  let addsub =
    skip_spaces *> choice [ (char '+' >>| fun _ -> Add); (char '-' >>| fun _ -> Sub) ]
  in
  let relat =
    skip_spaces
    *> choice
         [ (string ">=" >>| fun _ -> GrtEq)
         ; (string "<=" >>| fun _ -> LessEq)
         ; (char '>' >>| fun _ -> Grt)
         ; (char '<' >>| fun _ -> Less)
         ]
  in
  let equality =
    skip_spaces
    *> choice [ (string "=" >>| fun _ -> Eq); (string "<>" >>| fun _ -> NotEq) ]
  in
  let andalso = skip_spaces *> (string "andalso" >>| fun _ -> And) in
  let orelse = skip_spaces *> (string "orelse" >>| fun _ -> Or) in
  let rec parser_bin_op snd_exprarser op_parsers =
    let chainl1 snd_expr op_p =
      let rec go acc =
        lift2 (fun f x -> constr_binary_op f acc x) op_p snd_expr >>= go <|> return acc
      in
      snd_expr >>= fun init -> go init
    in
    match op_parsers with
    | [ op ] -> chainl1 snd_exprarser op
    | head :: tail -> chainl1 (parser_bin_op snd_exprarser tail) head
    | _ -> fail "Not a list."
  in
  fix (fun _ ->
    skip_spaces
    *>
    let helper =
      choice
        [ parens (s.snd_binary s)
        ; s.snd_unary s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parser_bin_op helper [ orelse; andalso; equality; relat; addsub; multdiv ])
;;

let parser_collection_helper brackets_parser constructor s =
  let separator = skip_spaces_rchar ',' *> skip_spaces <|> skip_spaces in
  fix (fun slf ->
    skip_spaces
    *>
    let helper =
      choice
        [ s.snd_tuple s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
        ; s.snd_let s
        ; s.snd_app s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf <|> lift constructor (brackets_parser (many (helper <* separator))))
;;

let snd_tuple s =
  let brackets parser = skip_spaces_rchar '(' *> parser <* skip_spaces_lchar ')' in
  parser_collection_helper brackets (fun b -> XTuple b) s
;;

let snd_list s =
  let brackets parser = skip_spaces_rchar '[' *> parser <* skip_spaces_lchar ']' in
  parser_collection_helper brackets (fun b -> XList b) s
;;

let snd_cons_list s =
  let separator = skip_spaces_rstr "::" *> skip_spaces in
  let constr_cons_list h tl = XConsList (h, tl) in
  fix (fun slf ->
    skip_spaces
    *>
    let helper =
      choice
        [ parens (s.snd_cons_list s)
        ; s.snd_unary s
        ; parens (s.snd_binary s)
        ; s.snd_tuple s
        ; s.snd_list s
        ; parens (s.snd_case s)
        ; s.snd_let s
        ; parens (s.snd_app s)
        ; parens (s.snd_arrfun s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf <|> lift2 constr_cons_list (helper <* separator) (slf <|> helper))
;;

let snd_case s =
  let constr_casexpr_of h tl = XCaseOf (h, tl) in
  fix (fun slf ->
    skip_spaces
    *>
    let helper_left =
      choice
        [ s.snd_unary s
        ; s.snd_cons_list s
        ; s.snd_tuple s
        ; s.snd_list s
        ; snd_liter
        ; snd_identifier
        ]
    in
    let helper_right =
      choice
        [ s.snd_case s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_cons_list s
        ; s.snd_tuple s
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
             constr_casexpr_of
             helper_right
             (let parser_case =
                lift2
                  (fun case action -> case, action)
                  helper_left
                  (skip_spaces_rstr "=>" *> helper_right)
              in
              skip_spaces_rstr "of"
              *> (skip_spaces_rstr "|" <|> skip_spaces)
              *> sep_by1 (skip_spaces_rstr "|") parser_case))
;;

let snd_let s =
  let constr_let_in h tl = XLetIn (h, tl) in
  let skip_let_spaces1 =
    string "let"
    *> take_while1 (function
         | ' ' | '\t' | '\n' | '\r' -> true
         | _ -> false)
  in
  fix (fun slf ->
    skip_spaces
    *>
    let helper =
      choice
        [ s.snd_let s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuple s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    parens slf
    <|> skip_let_spaces1
        *> lift2
             constr_let_in
             (many1 (s.snd_val_dec s <|> s.snd_val_rec_dec s))
             (skip_spaces_rstr "in" *> helper <* skip_spaces_rstr "end"))
;;

let snd_app s =
  let constr_app func_expr args = XApplication (func_expr, args) in
  fix (fun slf ->
    skip_spaces
    *>
    let parser_func =
      choice
        [ parens (s.snd_case s)
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
        ; parens (s.snd_tuple s)
        ; s.snd_list s
        ; parens (s.snd_cons_list s)
        ; parens (s.snd_case s)
        ; parens (s.snd_let s)
        ; parens (s.snd_arrfun s)
        ; parens (s.snd_if_then_else s)
        ; snd_liter
        ; snd_identifier
        ]
    in
    let apply_lift acc = lift (constr_app acc) parser_op in
    let rec go acc = apply_lift acc >>= go <|> return acc in
    parens slf <|> parser_func >>= fun init -> apply_lift init >>= fun init -> go init)
;;

let parser_value_declaration_helper keyword constructor s =
  let skip_key_skip = skip_spaces *> string keyword *> skip_spaces in
  let constr_id = function
    | XIdentifier x -> if x = "_" then fail "Wildcard not expected." else return x
    | _ -> fail "Unreachable variable."
  in
  fix (fun _ ->
    skip_key_skip
    *>
    let helper =
      choice
        [ s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuple s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
        ; s.snd_let s
        ; s.snd_app s
        ; s.snd_arrfun s
        ; s.snd_if_then_else s
        ; snd_liter
        ; snd_identifier
        ]
    in
    lift2 constructor (snd_identifier >>= constr_id) (skip_spaces_rstr "=" *> helper))
;;

let snd_val_dec s =
  let constr_val_dec_rec value_expr expr = XValDec (value_expr, expr) in
  parser_value_declaration_helper "val" constr_val_dec_rec s
;;

let snd_val_rec_dec s =
  let constr_val_dec_rec value_expr expr = XValRecDec (value_expr, expr) in
  parser_value_declaration_helper "val rec" constr_val_dec_rec s
;;

let snd_arrfun s =
  let skip_arr_skip = skip_spaces_lstr "=>" <* skip_spaces in
  let constr_arrfun h tl = XArrowFun (h, tl) in
  let constr_id = function
    | XIdentifier x -> return [ x ]
    | _ -> fail "Unreachable variable."
  in
  fix (fun slf ->
    skip_spaces
    *>
    let helper =
      choice
        [ s.snd_arrfun s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuple s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
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
             constr_arrfun
             (snd_identifier >>= constr_id <* skip_arr_skip)
             (helper <* skip_spaces))
;;

let snd_if_then_else s =
  let constr_if_then_else if_expr then_expr else_expr =
    XIfThenElse (if_expr, then_expr, else_expr)
  in
  fix (fun slf ->
    skip_spaces
    *>
    let helper =
      choice
        [ s.snd_if_then_else s
        ; s.snd_unary s
        ; s.snd_binary s
        ; s.snd_tuple s
        ; s.snd_list s
        ; s.snd_cons_list s
        ; s.snd_case s
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
             constr_if_then_else
             helper
             (skip_spaces_rstr "then" *> helper)
             (skip_spaces_rstr "else" *> helper))
;;

let snd_expr s =
  choice
    [ s.snd_unary s
    ; s.snd_binary s
    ; s.snd_tuple s
    ; s.snd_list s
    ; s.snd_cons_list s
    ; s.snd_case s
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
  ; snd_tuple
  ; snd_list
  ; snd_cons_list
  ; snd_case
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
