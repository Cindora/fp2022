(** Copyright 2022-2023, Grigory Aseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base
module Format = Caml.Format

let parse_str p s = parse_string ~consume:All p s

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_aletter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_cletter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

let is_id_char = function
  | c -> is_digit c || is_aletter c || is_cletter c || is_underscore c
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "in"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "function" -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let empty = take_while is_whitespace
let empty1 = take_while1 is_whitespace
let trim s = empty *> s <* empty
let token s = empty *> s
let token1 s = empty1 *> s
let str_token s = empty *> string s
let str_token1 s = empty1 *> string s
let pparens p = str_token "(" *> p <* str_token ")"
let pquotes p = str_token "\"" *> p <* str_token "\""
let pbrackets p = str_token "[" *> p <* str_token "]"
let pbanana_clips p = str_token1 "(|" *> p <* str_token "|)"
let pverticalbar = str_token "|"
let pcomma = str_token ","
let pdsemicolon = str_token "::"
let parrow = str_token "->"
let pbinding = str_token "let"
let pwild = str_token "_"

(**  Const constructors  *)
let constr_cint n = CInt n

let constr_cbool b = CBool b
let constr_cstring s = CString s

(**  Const parsers  *)
let pcint =
  let ps = token (option "" (str_token "-" <|> str_token "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> constr_cint (Int.of_string @@ sign ^ digit)) ps pd
;;

let pcbool =
  lift (fun b -> constr_cbool @@ Bool.of_string b) (str_token "false" <|> str_token "true")
;;

let pcstring =
  lift
    (fun s -> constr_cstring s)
    (pquotes @@ take_while (fun c -> not (Char.equal c '"')))
;;

let pcnil = pbrackets @@ (str_token "" *> return CNil)
let pcunit = pparens @@ (str_token "" *> return CUnit)
let pconst = token (choice [ pcint; pcbool; pcstring; pcnil; pcunit ])

(**  Identifier parsers  *)
let ident is_good_entry =
  let pchar = satisfy is_id_char in
  empty *> satisfy is_good_entry
  >>= fun h ->
  many pchar
  >>= fun tl ->
  let id = String.of_char_list (h :: tl) in
  if is_keyword id
  then fail ("Unexpected keyword '" ^ id ^ "' in binding")
  else if String.equal id "_"
  then fail "Wildcard \"_\" not expected"
  else return id
;;

let pident =
  let is_entry = function
    | c -> is_aletter c || is_underscore c
  in
  ident is_entry
;;

let pident_constr =
  let is_constr_entry = function
    | c -> is_cletter c
  in
  ident is_constr_entry
;;

(**  Pattern constructors  *)

let constr_pwild _ = PWild
let constr_pconst c = PConst c
let constr_pvar id = PVar id

let constr_pcons =
  List.fold_right ~init:(PConst CNil) ~f:(fun pat1 pat2 -> PCons (pat1, pat2))
;;

let constr_ptuple pats = PTuple pats
let constr_plist pats = PList pats
let constr_pacase id pats = PACase (id, pats)

(**  Pattern parsers  *)

let ppwild = constr_pwild <$> pwild
let ppconst = constr_pconst <$> pconst
let ppvar = constr_pvar <$> pident
let plist ps = pbrackets (sep_by1 (str_token ";") ps)

let sep_parser ps constr between =
  lift2 (fun pat pats -> constr @@ (pat :: pats)) (token @@ ps) (many1 (between *> ps))
;;

let ptuple ps constr =
  sep_parser ps constr pcomma <|> pparens @@ sep_parser ps constr pcomma
;;

let pconstr_without_args constr = token pident_constr >>= fun id -> return (constr id [])

let pconstr_with_args ps constr =
  lift2 (fun id args -> constr id args) (token pident_constr) (many1 ps)
;;

let pparens_only ps = pparens @@ choice ps

type pdispatch =
  { value : pdispatch -> pattern t
  ; cons : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; list : pdispatch -> pattern t
  ; acase_with_args : pdispatch -> pattern t
  ; acase_without_args : pdispatch -> pattern t
  ; pat : pdispatch -> pattern t
  }

let pack =
  let pat pack =
    choice
      [ pack.cons pack
      ; pack.tuple pack
      ; pack.list pack
      ; pack.acase_with_args pack
      ; pack.acase_without_args pack
      ; pack.value pack
      ]
  in
  let parsers pack =
    choice
      [ pack.value pack
      ; pack.list pack
      ; pparens @@ pack.tuple pack
      ; pack.acase_without_args pack
      ]
  in
  let value pack =
    fix @@ fun _ -> choice [ ppwild; ppconst; ppvar ] <|> pparens @@ pack.value pack
  in
  let cons pack =
    fix
    @@ fun _ ->
    sep_parser
      (parsers pack <|> pparens @@ pack.cons pack <|> pack.acase_with_args pack)
      constr_pcons
      pdsemicolon
    <|> pparens @@ pack.cons pack
  in
  let tuple pack =
    fix
    @@ fun _ ->
    ptuple (parsers pack <|> pack.cons pack <|> pack.acase_with_args pack) constr_ptuple
    <|> pparens @@ pack.tuple pack
  in
  let list pack =
    fix @@ fun _ -> constr_plist <$> plist @@ pack.pat pack <|> pparens @@ pack.list pack
  in
  let acase_without_args pack =
    fix
    @@ fun _ ->
    pconstr_without_args constr_pacase <|> pparens @@ pack.acase_without_args pack
  in
  let acase_with_args pack =
    fix
    @@ fun _ ->
    pconstr_with_args
      (parsers pack <|> pparens_only [ pack.cons pack; pack.acase_with_args pack ])
      constr_pacase
    <|> pparens @@ pack.acase_with_args pack
  in
  { value; cons; tuple; list; acase_with_args; acase_without_args; pat }
;;

let pattern = pack.pat pack

let pargs =
  choice
    [ pack.list pack
    ; pack.value pack
    ; pack.acase_without_args pack
    ; pparens_only [ pack.cons pack; pack.tuple pack; pack.acase_with_args pack ]
    ]
;;

(**  Operation constructor  *)
let ebinop binary_op expr1 expr2 = EBinOp (binary_op, expr1, expr2)

(**  Operation parsers  *)

let pop ch op = str_token ch *> return (ebinop op)
let pmulti = choice [ pop "*" Mul; pop "/" Div; pop "%" Mod ]
let pcons = pop "::" Cons
let padd = pop "+" Add <|> pop "-" Sub
let pcomp = choice [ pop ">=" Geq; pop ">" Gre; pop "<=" Leq; pop "<" Less ]
let peq = pop "=" Eq <|> pop "<>" Neq
let pconj = pop "&&" And
let pdisj = pop "||" Or

(**  Expr constructors  *)

let constr_econst e = EConst e
let constr_ebinop op e1 e2 = EBinOp (op, e1, e2)
let constr_evar id = EVar id
let constr_elist l = EList l
let constr_etuple t = ETuple t
let constr_eif e1 e2 e3 = EIf (e1, e2, e3)
let constr_efun pl e = List.fold_right ~init:e ~f:(fun p e -> EFun (p, e)) pl
let constr_elet r f e = ELet (r, f, e)
let constr_eletin elet e = ELetIn (elet, e)
let constr_eapp f args = List.fold_left ~init:f ~f:(fun f arg -> EApp (f, arg)) args
let constr_ematch e cases = EMatch (e, cases)
let constr_eapattern id args = EAPattern (id, args)
let constr_case pat expr = pat, expr
let constr_single_pat is_option constr = FAPattern (SingleChoice (is_option, constr))
let constr_multy_pat constrs = FAPattern (MultipleChoice constrs)

(** Expr parsers  *)

type edispatch =
  { evar : edispatch -> expr t
  ; econst : edispatch -> expr t
  ; op : edispatch -> expr t
  ; list : edispatch -> expr t
  ; tuple : edispatch -> expr t
  ; condition : edispatch -> expr t
  ; func : edispatch -> expr t
  ; bind : edispatch -> expr t
  ; bind_in : edispatch -> expr t
  ; app : edispatch -> expr t
  ; matching : edispatch -> expr t
  ; apattern_without_args : edispatch -> expr t
  ; apattern_with_args : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let peconst = constr_econst <$> pconst
let pevar = pident >>| constr_evar
let pfun_args = fix @@ fun p -> many pargs <|> pparens @@ p
let pfun_args1 = fix @@ fun p -> many1 pargs <|> pparens @@ p

let plet_body pargs pexpr =
  token1 pargs
  >>= fun args -> str_token "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let plet_body_without_args pexpr =
  str_token "=" *> pexpr >>| fun expr -> constr_efun [] expr
;;

let rec is_dublicate_constr = function
  | [] -> false
  | h :: tl -> List.exists tl ~f:(fun x -> String.equal h x) || is_dublicate_constr tl
;;

let pack =
  let expr_parsers_without_matching pack =
    choice
      [ pack.op pack
      ; pack.tuple pack
      ; pack.app pack
      ; pack.list pack
      ; pack.condition pack
      ; pack.func pack
      ; pack.bind_in pack
      ; pack.apattern_with_args pack
      ; pack.apattern_without_args pack
      ]
  in
  let expr_parsers pack = expr_parsers_without_matching pack <|> pack.matching pack in
  let matching_parsers pack =
    expr_parsers_without_matching pack <|> pparens @@ pack.matching pack
  in
  let expr pack = expr_parsers pack <|> pack.bind pack in
  let econst pack = fix @@ fun _ -> peconst <|> pparens @@ pack.econst pack in
  let evar pack = fix @@ fun _ -> pevar <|> pparens @@ pack.evar pack in
  let op_parsers pack =
    choice
      [ pack.list pack
      ; pack.bind_in pack
      ; pack.app pack
      ; pack.apattern_with_args pack
      ; pack.apattern_without_args pack
      ; pparens_only
          [ pack.tuple pack; pack.op pack; pack.condition pack; pack.matching pack ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let list_parsers pack =
    choice
      [ pack.op pack
      ; pack.list pack
      ; pack.tuple pack
      ; pack.condition pack
      ; pack.app pack
      ; pack.apattern_with_args pack
      ; pack.apattern_without_args pack
      ; pparens_only [ pack.matching pack; pack.func pack; pack.bind_in pack ]
      ]
  in
  let tuple_parsers pack =
    choice
      [ pack.op pack
      ; pack.list pack
      ; pack.bind_in pack
      ; pack.app pack
      ; pack.apattern_with_args pack
      ; pack.apattern_without_args pack
      ; pparens_only
          [ pack.func pack; pack.tuple pack; pack.matching pack; pack.condition pack ]
      ]
  in
  let acase_parsers pack =
    choice
      [ pack.list pack
      ; pack.apattern_without_args pack
      ; pparens_only
          [ pack.apattern_with_args pack
          ; pack.op pack
          ; pack.tuple pack
          ; pack.condition pack
          ; pack.func pack
          ; pack.matching pack
          ; pack.app pack
          ; pack.bind_in pack
          ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let cond_bool_parsers pack =
    choice
      [ pack.op pack
      ; pack.bind_in pack
      ; pack.app pack
      ; pack.condition pack
      ; pack.matching pack
      ]
  in
  let app_func_parsers pack =
    choice
      [ pack.evar pack
      ; pparens_only
          [ pack.condition pack
          ; pack.func pack
          ; pack.matching pack
          ; pack.app pack
          ; pack.bind_in pack
          ]
      ]
  in
  let app_args_parsers pack =
    choice
      [ pack.list pack
      ; pack.apattern_without_args pack
      ; pparens_only
          [ pack.op pack
          ; pack.tuple pack
          ; pack.condition pack
          ; pack.matching pack
          ; pack.func pack
          ; pack.app pack
          ; pack.bind_in pack
          ; pack.apattern_with_args pack
          ]
      ; pack.evar pack
      ; pack.econst pack
      ]
  in
  let op pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let cons = chainr1 add pcons in
    let comp = chainl1 cons pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <|> pparens @@ pack.op pack
  in
  let list pack =
    fix
    @@ fun _ -> constr_elist <$> plist @@ list_parsers pack <|> pparens @@ pack.list pack
  in
  let tuple pack =
    fix
    @@ fun _ -> ptuple (tuple_parsers pack) constr_etuple <|> pparens @@ pack.tuple pack
  in
  let apattern_without_args pack =
    fix
    @@ fun _ ->
    pconstr_without_args constr_eapattern <|> pparens @@ pack.apattern_without_args pack
  in
  let apattern_with_args pack =
    fix
    @@ fun _ ->
    pconstr_with_args (acase_parsers pack) constr_eapattern
    <|> pparens @@ pack.apattern_with_args pack
  in
  let condition pack =
    fix
    @@ fun _ ->
    lift3
      constr_eif
      (str_token "if" *> cond_bool_parsers pack)
      (str_token1 "then" *> expr_parsers pack)
      (str_token1 "else" *> expr_parsers pack)
    <|> pparens @@ pack.condition pack
  in
  let func pack =
    fix
    @@ fun _ ->
    lift2 constr_efun (str_token "fun" *> pfun_args1) (parrow *> expr_parsers pack)
    <|> pparens @@ pack.func pack
  in
  let app pack =
    fix
    @@ fun _ ->
    lift2 constr_eapp (app_func_parsers pack) (many1 (token1 @@ app_args_parsers pack))
    <|> pparens @@ pack.app pack
  in
  let matching pack =
    fix
    @@ fun _ ->
    lift2
      constr_ematch
      (str_token "match" *> matching_parsers pack <* str_token1 "with")
      (let case2 =
         lift2 constr_case (pverticalbar *> pattern <* parrow) (matching_parsers pack)
       in
       let case1 = lift2 constr_case (pattern <* parrow) (matching_parsers pack) in
       let cases = lift2 (fun h tl -> h :: tl) (case1 <|> case2) (many case2) in
       cases)
    <|> pparens @@ pack.matching pack
  in
  let plain_bind pack =
    fix
    @@ fun _ ->
    lift3
      constr_elet
      (pbinding *> option false (str_token1 "rec " >>| fun _ -> true))
      (pident >>| fun f -> FName f)
      (plet_body pfun_args (expr_parsers pack))
    <|> lift3
          constr_elet
          (pbinding *> return false)
          (str_token1 "_" *> return (FName "_"))
          (plet_body_without_args (expr_parsers pack))
  in
  let bind_single_apattern pack =
    fix
    @@ fun _ ->
    lift3
      constr_elet
      (pbinding *> return false)
      (pbanana_clips
         (pident_constr
         >>= fun id ->
         option false (pverticalbar *> pwild >>| fun _ -> true)
         >>| fun is_opt -> constr_single_pat is_opt id))
      (plet_body pfun_args1 (expr_parsers pack))
  in
  let bind_multy_apattern pack =
    fix
    @@ fun _ ->
    lift3
      constr_elet
      (pbinding *> return false)
      (pbanana_clips
         (pident_constr
         >>= fun fst ->
         many1 (pverticalbar *> pident_constr)
         >>= fun tl ->
         if List.length tl < 7
         then
           if not @@ is_dublicate_constr (fst :: tl)
           then return (constr_multy_pat @@ (fst :: tl))
           else fail "Active patterns cannot contain the same constructors"
         else fail "Active patterns cannot return more than 7 possibilities"))
      (plet_body (pargs >>| fun value_to_match -> [ value_to_match ]) (expr_parsers pack))
  in
  let bind pack =
    fix
    @@ fun _ ->
    choice [ bind_single_apattern pack; bind_multy_apattern pack; plain_bind pack ]
    <|> pparens @@ pack.bind pack
  in
  let bind_in pack =
    fix
    @@ fun _ ->
    lift2 constr_eletin (plain_bind pack) (str_token1 "in" *> expr_parsers pack)
    <|> pparens @@ pack.bind_in pack
  in
  { evar
  ; econst
  ; op
  ; list
  ; tuple
  ; condition
  ; func
  ; bind
  ; bind_in
  ; app
  ; matching
  ; apattern_without_args
  ; apattern_with_args
  ; expr
  }
;;

let expr = pack.expr pack

(**  Program parser  *)
let pprogram = many1 (token expr <* token (many1 (str_token ";;")))

let parse str = parse_str pprogram (String.strip str)

(**  TESTS  *)

let show_parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "%s" e
;;

(**  Consts tests  *)

let%expect_test _ =
  show_parsed_result "777" pconst show_const;
  [%expect {| (CInt 777) |}]
;;

let%expect_test _ =
  show_parsed_result "-777" pconst show_const;
  [%expect {| (CInt -777) |}]
;;

let%expect_test _ =
  show_parsed_result "true" pconst show_const;
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  show_parsed_result "false" pconst show_const;
  [%expect {| (CBool false) |}]
;;

let%expect_test _ =
  show_parsed_result "\"May the power be with us\"" pconst show_const;
  [%expect {| (CString "May the power be with us") |}]
;;

let%expect_test _ =
  show_parsed_result "[]" pconst show_const;
  [%expect {| CNil |}]
;;

let%expect_test _ =
  show_parsed_result "()" pconst show_const;
  [%expect {| CUnit |}]
;;

(**  Pattern tests  *)

let%expect_test _ =
  show_parsed_result "777" pattern show_pattern;
  [%expect {|
    (PConst (CInt 777)) |}]
;;

let%expect_test _ =
  show_parsed_result "a" pattern show_pattern;
  [%expect {|
    (PVar "a") |}]
;;

let%expect_test _ =
  show_parsed_result "[]" pattern show_pattern;
  [%expect {|
    (PConst CNil) |}]
;;

let%expect_test _ =
  show_parsed_result "[1;2;3]" pattern show_pattern;
  [%expect {|
    (PList [(PConst (CInt 1)); (PConst (CInt 2)); (PConst (CInt 3))]) |}]
;;

let%expect_test _ =
  show_parsed_result "a :: b" pattern show_pattern;
  [%expect {|
    (PCons ((PVar "a"), (PCons ((PVar "b"), (PConst CNil))))) |}]
;;

let%expect_test _ =
  show_parsed_result "a :: b :: []" pattern show_pattern;
  [%expect
    {|
    (PCons ((PVar "a"),
       (PCons ((PVar "b"), (PCons ((PConst CNil), (PConst CNil))))))) |}]
;;

let%expect_test _ =
  show_parsed_result "a :: b :: [c;d]" pattern show_pattern;
  [%expect
    {|
    (PCons ((PVar "a"),
       (PCons ((PVar "b"),
          (PCons ((PList [(PVar "c"); (PVar "d")]), (PConst CNil)))))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result "( 1 , 2)" pattern show_pattern;
  [%expect {|
    (PTuple [(PConst (CInt 1)); (PConst (CInt 2))]) |}]
;;

let%expect_test _ =
  show_parsed_result "a , b" pattern show_pattern;
  [%expect {|
    (PTuple [(PVar "a"); (PVar "b")]) |}]
;;

let%expect_test _ =
  show_parsed_result "()" pattern show_pattern;
  [%expect {|
    (PConst CUnit) |}]
;;

let%expect_test _ =
  show_parsed_result "_" pattern show_pattern;
  [%expect {|
    PWild |}]
;;

let%expect_test _ =
  show_parsed_result "( 1, 2) :: d" pattern show_pattern;
  [%expect
    {|
    (PCons ((PTuple [(PConst (CInt 1)); (PConst (CInt 2))]),
       (PCons ((PVar "d"), (PConst CNil))))) |}]
;;

let%expect_test _ =
  show_parsed_result "(a :: b ) :: c" pattern show_pattern;
  [%expect
    {|
    (PCons ((PCons ((PVar "a"), (PCons ((PVar "b"), (PConst CNil))))),
       (PCons ((PVar "c"), (PConst CNil))))) |}]
;;

let%expect_test _ =
  show_parsed_result "[(a, b); (c, d)]" pattern show_pattern;
  [%expect
    {|
    (PList [(PTuple [(PVar "a"); (PVar "b")]); (PTuple [(PVar "c"); (PVar "d")])]) |}]
;;

let%expect_test _ =
  show_parsed_result "[1;2], [3;4], [5;6]" pattern show_pattern;
  [%expect
    {|
    (PTuple
       [(PList [(PConst (CInt 1)); (PConst (CInt 2))]);
         (PList [(PConst (CInt 3)); (PConst (CInt 4))]);
         (PList [(PConst (CInt 5)); (PConst (CInt 6))])]) |}]
;;

let%expect_test _ =
  show_parsed_result "([1;2], [3;4], [5;6])" pattern show_pattern;
  [%expect
    {|
    (PTuple
       [(PList [(PConst (CInt 1)); (PConst (CInt 2))]);
         (PList [(PConst (CInt 3)); (PConst (CInt 4))]);
         (PList [(PConst (CInt 5)); (PConst (CInt 6))])]) |}]
;;

let%expect_test _ =
  show_parsed_result "[[1; 2]; [3;4;5]]" pattern show_pattern;
  [%expect
    {|
    (PList
       [(PList [(PConst (CInt 1)); (PConst (CInt 2))]);
         (PList [(PConst (CInt 3)); (PConst (CInt 4)); (PConst (CInt 5))])]) |}]
;;

let%expect_test _ =
  show_parsed_result "1, (a, b), (c, d, e), 3" pattern show_pattern;
  [%expect
    {|
    (PTuple
       [(PConst (CInt 1)); (PTuple [(PVar "a"); (PVar "b")]);
         (PTuple [(PVar "c"); (PVar "d"); (PVar "e")]); (PConst (CInt 3))]) |}]
;;

let%expect_test _ =
  show_parsed_result "Even x" pattern show_pattern;
  [%expect {|
    (PACase ("Even", [(PVar "x")])) |}]
;;

let%expect_test _ =
  show_parsed_result "RGB(r, g, b)" pattern show_pattern;
  [%expect {|
    (PACase ("RGB", [(PTuple [(PVar "r"); (PVar "g"); (PVar "b")])])) |}]
;;

let%expect_test _ =
  show_parsed_result "None" pattern show_pattern;
  [%expect {|
    (PACase ("None", [])) |}]
;;

let%expect_test _ =
  show_parsed_result "(((1)))" pattern show_pattern;
  [%expect {|
    (PConst (CInt 1)) |}]
;;

let%expect_test _ =
  show_parsed_result "1 , (([1]))" pattern show_pattern;
  [%expect {|
    (PTuple [(PConst (CInt 1)); (PList [(PConst (CInt 1))])]) |}]
;;

(**  Expression tests  *)

(**  Binary operations  *)

let%expect_test _ =
  show_parsed_result "x + 1" expr show_expr;
  [%expect {|
    (EBinOp (Add, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x - 1" expr show_expr;
  [%expect {|
    (EBinOp (Sub, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x + y - 1" expr show_expr;
  [%expect
    {|
    (EBinOp (Sub, (EBinOp (Add, (EVar "x"), (EVar "y"))), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x * y / z" expr show_expr;
  [%expect {|
    (EBinOp (Div, (EBinOp (Mul, (EVar "x"), (EVar "y"))), (EVar "z"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x * (y / z)" expr show_expr;
  [%expect {|
    (EBinOp (Mul, (EVar "x"), (EBinOp (Div, (EVar "y"), (EVar "z"))))) |}]
;;

let%expect_test _ =
  show_parsed_result "(x + y) * (z - y) / (y * x)  *  m" expr show_expr;
  [%expect
    {|
    (EBinOp (Mul,
       (EBinOp (Div,
          (EBinOp (Mul, (EBinOp (Add, (EVar "x"), (EVar "y"))),
             (EBinOp (Sub, (EVar "z"), (EVar "y"))))),
          (EBinOp (Mul, (EVar "y"), (EVar "x"))))),
       (EVar "m"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x % 100" expr show_expr;
  [%expect {|
    (EBinOp (Mod, (EVar "x"), (EConst (CInt 100)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x < 1" expr show_expr;
  [%expect {|
    (EBinOp (Less, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x > 1" expr show_expr;
  [%expect {|
    (EBinOp (Gre, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x <= 1" expr show_expr;
  [%expect {|
    (EBinOp (Leq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x >= 1" expr show_expr;
  [%expect {|
    (EBinOp (Geq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x = y" expr show_expr;
  [%expect {|
    (EBinOp (Eq, (EVar "x"), (EVar "y"))) |}]
;;

let%expect_test _ =
  show_parsed_result "x <> 1" expr show_expr;
  [%expect {|
    (EBinOp (Neq, (EVar "x"), (EConst (CInt 1)))) |}]
;;

let%expect_test _ =
  show_parsed_result "true || x > 1 || y > 1 || false" expr show_expr;
  [%expect
    {|
    (EBinOp (Or,
       (EBinOp (Or,
          (EBinOp (Or, (EConst (CBool true)),
             (EBinOp (Gre, (EVar "x"), (EConst (CInt 1)))))),
          (EBinOp (Gre, (EVar "y"), (EConst (CInt 1)))))),
       (EConst (CBool false)))) |}]
;;

let%expect_test _ =
  show_parsed_result "x && y || z && w" expr show_expr;
  [%expect
    {|
    (EBinOp (Or, (EBinOp (And, (EVar "x"), (EVar "y"))),
       (EBinOp (And, (EVar "z"), (EVar "w"))))) |}]
;;

let%expect_test _ =
  show_parsed_result "x || y && z || w" expr show_expr;
  [%expect
    {|
    (EBinOp (Or,
       (EBinOp (Or, (EVar "x"), (EBinOp (And, (EVar "y"), (EVar "z"))))),
       (EVar "w"))) |}]
;;

let%expect_test _ =
  show_parsed_result "0 :: tl" expr show_expr;
  [%expect {|
    (EBinOp (Cons, (EConst (CInt 0)), (EVar "tl"))) |}]
;;

let%expect_test _ =
  show_parsed_result "a :: b :: c :: tl" expr show_expr;
  [%expect
    {|
    (EBinOp (Cons, (EVar "a"),
       (EBinOp (Cons, (EVar "b"), (EBinOp (Cons, (EVar "c"), (EVar "tl"))))))) |}]
;;

(**  Lists  *)

let%expect_test _ =
  show_parsed_result "[a; b; c; d]" expr show_expr;
  [%expect {|
    (EList [(EVar "a"); (EVar "b"); (EVar "c"); (EVar "d")]) |}]
;;

let%expect_test _ =
  show_parsed_result "[[a;b]; [c;d]]" expr show_expr;
  [%expect
    {|
    (EList [(EList [(EVar "a"); (EVar "b")]); (EList [(EVar "c"); (EVar "d")])]) |}]
;;

let%expect_test _ =
  show_parsed_result "[a + b; c * d]" expr show_expr;
  [%expect
    {|
    (EList
       [(EBinOp (Add, (EVar "a"), (EVar "b")));
         (EBinOp (Mul, (EVar "c"), (EVar "d")))]) |}]
;;

let%expect_test _ =
  show_parsed_result "[(a + b, c + d); (1 * 2, 3 * 4)]" expr show_expr;
  [%expect
    {|
    (EList
       [(ETuple
           [(EBinOp (Add, (EVar "a"), (EVar "b")));
             (EBinOp (Add, (EVar "c"), (EVar "d")))]);
         (ETuple
            [(EBinOp (Mul, (EConst (CInt 1)), (EConst (CInt 2))));
              (EBinOp (Mul, (EConst (CInt 3)), (EConst (CInt 4))))])
         ]) |}]
;;

(**  Tuples *)

let%expect_test _ =
  show_parsed_result "(1 + x, [2; 4 + x], 3)" expr show_expr;
  [%expect
    {|
    (ETuple
       [(EBinOp (Add, (EConst (CInt 1)), (EVar "x")));
         (EList
            [(EConst (CInt 2)); (EBinOp (Add, (EConst (CInt 4)), (EVar "x")))]);
         (EConst (CInt 3))]) |}]
;;

let%expect_test _ =
  show_parsed_result "1, 2, 3" expr show_expr;
  [%expect {|
    : end_of_input |}]
;;

(**  Conditions  *)

let%expect_test _ =
  show_parsed_result "if true then 1 else 2" expr show_expr;
  [%expect {|
    (EIf ((EConst (CBool true)), (EConst (CInt 1)), (EConst (CInt 2)))) |}]
;;

let%expect_test _ =
  show_parsed_result "if x % 5 = 0 then x * 5 else (x - x % 5) * 5" expr show_expr;
  [%expect
    {|
    (EIf (
       (EBinOp (Eq, (EBinOp (Mod, (EVar "x"), (EConst (CInt 5)))),
          (EConst (CInt 0)))),
       (EBinOp (Mul, (EVar "x"), (EConst (CInt 5)))),
       (EBinOp (Mul,
          (EBinOp (Sub, (EVar "x"), (EBinOp (Mod, (EVar "x"), (EConst (CInt 5))))
             )),
          (EConst (CInt 5))))
       )) |}]
;;

(**  Lambda functions  *)

let%expect_test _ =
  show_parsed_result "fun x -> x" expr show_expr;
  [%expect {|
    (EFun ((PVar "x"), (EVar "x"))) |}]
;;

let%expect_test _ =
  show_parsed_result
    "fun x y z-> fun t -> if t > 0 then (x + y + z - t) / t else 0"
    expr
    show_expr;
  [%expect
    {|
    (EFun ((PVar "x"),
       (EFun ((PVar "y"),
          (EFun ((PVar "z"),
             (EFun ((PVar "t"),
                (EIf ((EBinOp (Gre, (EVar "t"), (EConst (CInt 0)))),
                   (EBinOp (Div,
                      (EBinOp (Sub,
                         (EBinOp (Add, (EBinOp (Add, (EVar "x"), (EVar "y"))),
                            (EVar "z"))),
                         (EVar "t"))),
                      (EVar "t"))),
                   (EConst (CInt 0))))
                ))
             ))
          ))
       )) |}]
;;

(**  Applications  *)

let%expect_test _ =
  show_parsed_result "f x" expr show_expr;
  [%expect {|
    (EApp ((EVar "f"), (EVar "x"))) |}]
;;

let%expect_test _ =
  show_parsed_result "(fun x y z -> x * 100) x y z" expr show_expr;
  [%expect
    {|
    (EApp (
       (EApp (
          (EApp (
             (EFun ((PVar "x"),
                (EFun ((PVar "y"),
                   (EFun ((PVar "z"),
                      (EBinOp (Mul, (EVar "x"), (EConst (CInt 100))))))
                   ))
                )),
             (EVar "x"))),
          (EVar "y"))),
       (EVar "z"))) |}]
;;

let%expect_test _ =
  show_parsed_result "g x 1 [1;2] y z" expr show_expr;
  [%expect
    {|
    (EApp (
       (EApp (
          (EApp ((EApp ((EApp ((EVar "g"), (EVar "x"))), (EConst (CInt 1)))),
             (EList [(EConst (CInt 1)); (EConst (CInt 2))]))),
          (EVar "y"))),
       (EVar "z"))) |}]
;;

(**  Pattern matching  *)

let%expect_test _ =
  show_parsed_result "match 2 with | 1 -> 1 | _ -> 5" expr show_expr;
  [%expect
    {|
    (EMatch ((EConst (CInt 2)),
       [((PConst (CInt 1)), (EConst (CInt 1))); (PWild, (EConst (CInt 5)))])) |}]
;;

let%expect_test _ =
  show_parsed_result "match x with Some x -> x | None -> 0" expr show_expr;
  [%expect
    {|
    (EMatch ((EVar "x"),
       [((PACase ("Some", [(PVar "x")])), (EVar "x"));
         ((PACase ("None", [])), (EConst (CInt 0)))]
       )) |}]
;;

(**  Active patterns application  *)

let%expect_test _ =
  show_parsed_result "Some x" expr show_expr;
  [%expect {|
    (EAPattern ("Some", [(EVar "x")])) |}]
;;

let%expect_test _ =
  show_parsed_result "Some (Some x)" expr show_expr;
  [%expect {|
    (EAPattern ("Some", [(EAPattern ("Some", [(EVar "x")]))])) |}]
;;

let%expect_test _ =
  show_parsed_result "None" expr show_expr;
  [%expect {|
    (EAPattern ("None", [])) |}]
;;

let%expect_test _ =
  show_parsed_result "Some None" expr show_expr;
  [%expect {|
    (EAPattern ("Some", [(EAPattern ("None", []))])) |}]
;;

let%expect_test _ =
  show_parsed_result "Default \"random citizen\" name" expr show_expr;
  [%expect
    {|
    (EAPattern ("Default", [(EConst (CString "random citizen")); (EVar "name")])) |}]
;;

(**  Let and let rec  *)

let%expect_test _ =
  show_parsed_result "let f x = x" expr show_expr;
  [%expect {|
    (ELet (false, (FName "f"), (EFun ((PVar "x"), (EVar "x"))))) |}]
;;

let%expect_test _ =
  show_parsed_result "let bind x f = f x" expr show_expr;
  [%expect
    {|
    (ELet (false, (FName "bind"),
       (EFun ((PVar "x"), (EFun ((PVar "f"), (EApp ((EVar "f"), (EVar "x")))))))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result "let mult x = fun y -> x * y" expr show_expr;
  [%expect
    {|
    (ELet (false, (FName "mult"),
       (EFun ((PVar "x"),
          (EFun ((PVar "y"), (EBinOp (Mul, (EVar "x"), (EVar "y")))))))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result "let x = 10" expr show_expr;
  [%expect {|
    (ELet (false, (FName "x"), (EConst (CInt 10)))) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec factorial n =\n\
    \  if n <= 0\n\
    \  then 1\n\
    \  else (\n\
    \    match n with\n\
    \    | 1 -> 1\n\
    \    | _ -> factorial (n - 1) * n)"
    expr
    show_expr;
  [%expect
    {|
    (ELet (true, (FName "factorial"),
       (EFun ((PVar "n"),
          (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 0)))), (EConst (CInt 1)),
             (EMatch ((EVar "n"),
                [((PConst (CInt 1)), (EConst (CInt 1)));
                  (PWild,
                   (EBinOp (Mul,
                      (EApp ((EVar "factorial"),
                         (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                      (EVar "n"))))
                  ]
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec series n = if n = 1 then 1 else n + series (n - 1)"
    expr
    show_expr;
  [%expect
    {|
    (ELet (true, (FName "series"),
       (EFun ((PVar "n"),
          (EIf ((EBinOp (Eq, (EVar "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
             (EBinOp (Add, (EVar "n"),
                (EApp ((EVar "series"),
                   (EBinOp (Sub, (EVar "n"), (EConst (CInt 1))))))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec fibonacci n = if n <= 1 then 1 else fibonacci (n - 1) + fibonacci (n - 2)"
    expr
    show_expr;
  [%expect
    {|
    (ELet (true, (FName "fibonacci"),
       (EFun ((PVar "n"),
          (EIf ((EBinOp (Leq, (EVar "n"), (EConst (CInt 1)))), (EConst (CInt 1)),
             (EBinOp (Add,
                (EApp ((EVar "fibonacci"),
                   (EBinOp (Sub, (EVar "n"), (EConst (CInt 1)))))),
                (EApp ((EVar "fibonacci"),
                   (EBinOp (Sub, (EVar "n"), (EConst (CInt 2))))))
                ))
             ))
          ))
       )) |}]
;;

(**  Let in and let rec in  *)

let%expect_test _ =
  show_parsed_result "let sum x = fun y -> x + y in sum 10 5" expr show_expr;
  [%expect
    {|
    (ELetIn (
       (ELet (false, (FName "sum"),
          (EFun ((PVar "x"),
             (EFun ((PVar "y"), (EBinOp (Add, (EVar "x"), (EVar "y")))))))
          )),
       (EApp ((EApp ((EVar "sum"), (EConst (CInt 10)))), (EConst (CInt 5)))))) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let rec fact x = if x <= 1 then 1 else fact (x - 1) * x in fact 10"
    expr
    show_expr;
  [%expect
    {|
    (ELetIn (
       (ELet (true, (FName "fact"),
          (EFun ((PVar "x"),
             (EIf ((EBinOp (Leq, (EVar "x"), (EConst (CInt 1)))),
                (EConst (CInt 1)),
                (EBinOp (Mul,
                   (EApp ((EVar "fact"),
                      (EBinOp (Sub, (EVar "x"), (EConst (CInt 1)))))),
                   (EVar "x")))
                ))
             ))
          )),
       (EApp ((EVar "fact"), (EConst (CInt 10)))))) |}]
;;

(**  Active pattern declaration  *)

let%expect_test _ =
  show_parsed_result
    "let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd"
    expr
    show_expr;
  [%expect
    {|
    (ELet (false, (FAPattern (MultipleChoice ["Even"; "Odd"])),
       (EFun ((PVar "input"),
          (EIf (
             (EBinOp (Eq, (EBinOp (Mod, (EVar "input"), (EConst (CInt 2)))),
                (EConst (CInt 0)))),
             (EAPattern ("Even", [])), (EAPattern ("Odd", []))))
          ))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let (|Default|) onNone value = \n\n\
    \    match value with\n\n\
    \      | None -> onNone\n\n\
    \      | Some e -> e"
    expr
    show_expr;
  [%expect
    {|
    (ELet (false, (FAPattern (SingleChoice (false, "Default"))),
       (EFun ((PVar "onNone"),
          (EFun ((PVar "value"),
             (EMatch ((EVar "value"),
                [((PACase ("None", [])), (EVar "onNone"));
                  ((PACase ("Some", [(PVar "e")])), (EVar "e"))]
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  show_parsed_result
    "let (| Foo|_|) s x = if x = s then Some Foo else None"
    expr
    show_expr;
  [%expect
    {|
    (ELet (false, (FAPattern (SingleChoice (true, "Foo"))),
       (EFun ((PVar "s"),
          (EFun ((PVar "x"),
             (EIf ((EBinOp (Eq, (EVar "x"), (EVar "s"))),
                (EAPattern ("Some", [(EAPattern ("Foo", []))])),
                (EAPattern ("None", []))))
             ))
          ))
       )) |}]
;;
