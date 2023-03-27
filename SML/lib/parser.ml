(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let parse p s = parse_string ~consume:All p s

let is_empty = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let not_is_empty c = is_empty c |> not
let empty = take_while is_empty
let empty_lr p = empty *> p <* empty

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_wild = function
  | '_' -> true
  | _ -> false
;;

let is_valid_ident c = is_digit c || is_lower c || is_upper c || is_wild c

let is_keyword = function
  | "let"
  | "in"
  | "true"
  | "false"
  | "rec"
  | "fn"
  | "if"
  | "else"
  | "then"
  | "div"
  | "end"
  | "val"
  | "of"
  | "orelse"
  | "andalso"
  | "fun"
  | "case" -> true
  | _ -> false
;;

let token s = empty *> string s

let keyword s =
  token s
  <* (peek_char
     >>| function
     | Some x when is_valid_ident x -> fail "Incorrect keyword"
     | _ -> return None)
;;

let between l r p = l *> p <* r
let brackets p = token "(" *> p <* token ")"
let cint n = CInt n
let cbool b = CBool b
let cstring s = CString s
let econst c = EConst c
let eunop o e = EUnOp (o, e)
let evar id = EVar id
let etuple l = ETuple l
let econs e1 e2 = ECons (e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet binds e = ELet (binds, e)
let efunction cases = EFun (PtVar "case", EMatch (EVar "case", cases))
let efun p e = EFun (p, e)
let eapp = return (fun e1 e2 -> EApp (e1, e2))
let ematch e cases = EMatch (e, cases)
let efun args rhs = List.fold_right args ~f:efun ~init:rhs
let ebinop o e1 e2 = EBinOp (o, e1, e2)
let elist = List.fold_right ~f:econs ~init:ENil
let ccase p e = p, e
let bbind isrec p e = isrec, p, e
let ptwild _ = PtWild
let ptvar id = PtVar id
let ptconst c = PtConst c
let pttuple l = PtTuple l
let popcons = token "::" *> return (fun p1 p2 -> PtCons (p1, p2))
let plist = List.fold_right ~f:(fun p1 p2 -> PtCons (p1, p2)) ~init:PtNil
let dlet isrec p e = DLet (isrec, p, e)

let procr op pl pr =
  let p =
    fix @@ fun p -> pl >>= fun l -> op >>= (fun op -> p <|> pr >>| op l) <|> return l
  in
  p
;;

let procl op pl pr =
  let rec go acc =
    (fun f x -> f acc x) <$> op <*> choice [ pl >>= go; pr ] <|> return acc
  in
  pl >>= go
;;

let choice_op ops =
  choice @@ List.map ~f:(fun (tok, cons) -> token tok *> (return @@ ebinop cons)) ops
;;

let cons = token "::" *> return econs

let apply_unary p =
  choice
    [ token "~" *> p >>| eunop Minus
    ; keyword "not" *> p >>| eunop Not
    ; token "+" *> p
    ; p
    ]
;;

let id valid_fst =
  let* fst = empty *> satisfy valid_fst in
  let take_func =
    match fst with
    | '_' -> many1
    | _ -> many
  in
  let* inner = take_func @@ satisfy is_valid_ident in
  let id = Base.String.of_char_list @@ (fst :: inner) in
  if is_keyword id then fail "Keyword" else return id
;;

let ident =
  id
  @@ function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let uns = empty_lr @@ take_while1 is_digit

let cunsint =
  let* uns = uns in
  return @@ Base.Int.of_string uns >>| cint
;;

let cint =
  let* sign = option "\r" (token "~") in
  let* uns = empty_lr @@ take_while1 is_digit in
  return @@ Base.Int.of_string ((if sign == "\r" then "" else "-") ^ uns) >>| cint
;;

let cbool =
  let ctrue = keyword "true" *> return (cbool true) in
  let cfalse = keyword "false" *> return (cbool false) in
  ctrue <|> cfalse
;;

let cstring =
  between (char '"') (char '"')
  @@ take_while (function
       | '"' -> false
       | _ -> true)
  >>| cstring
;;

let const = empty_lr @@ choice [ cint; cbool; cstring ]
let uns_const = empty_lr @@ choice [ cint; cbool; cstring ]
let ptvar = ident >>| ptvar
let ptwild = token "_" >>| ptwild
let ptconst = const >>| ptconst

type pdispatch =
  { tuple : pdispatch -> pt t
  ; other : pdispatch -> pt t
  ; pt : pdispatch -> pt t
  }

let chainl1 e op =
  let rec go acc = op >>| (fun f x -> f acc x) <*> e >>= go <|> return acc in
  e >>= go
;;

let pack =
  let pt d = fix @@ fun _self -> empty_lr @@ choice [ d.tuple d; d.other d ] in
  let tuple d =
    fix
    @@ fun _self ->
    empty_lr
      ((fun hd tl -> hd :: tl)
      <$> token "(" *> d.other d
      <*> many1 (token "," *> d.other d)
      <* token ")")
    >>| pttuple
  in
  let other d =
    fix
    @@ fun _self ->
    let pnone = token "NONE" *> return PtNone in
    let plist =
      empty_lr @@ between (token "[") (token "]") @@ sep_by (token ",") @@ d.pt d
      >>| plist
    in
    let prim =
      empty_lr @@ choice [ ptconst; pnone; ptvar; ptwild; plist; brackets @@ d.pt d ]
    in
    let prim_or_some = prim <|> (token "SOME" *> prim >>= fun x -> return @@ PtSome x) in
    chainl1 prim_or_some popcons
  in
  { other; pt; tuple }
;;

let pt = pack.pt pack

type edispatch =
  { key : edispatch -> exp t
  ; tuple : edispatch -> exp t
  ; exp : edispatch -> exp t
  ; op : edispatch -> exp t
  }

let chainl1' i e op =
  let rec go acc = op >>| (fun f x -> f acc x) <*> e >>= go <|> return acc in
  i >>= go
;;

let pack =
  let pt = pt in
  let exp d = fix @@ fun _self -> empty_lr @@ d.key d <|> d.tuple d <|> d.op d in
  let key d =
    fix
    @@ fun _self ->
    let eif =
      empty_lr
      @@ lift3
           eif
           (keyword "if" *> d.exp d)
           (keyword "then" *> d.exp d)
           (keyword "else" *> d.exp d)
    in
    let elet =
      let binding =
        empty_lr
          (bbind
          <$> keyword "val" *> option false (keyword "rec" >>| fun _ -> true)
          <*> pt
          <*> (efun <$> (empty *> many pt <* token "=") <*> d.exp d))
        <|> (bbind
            <$> keyword "fun" *> return true
            <*> pt
            <*> (efun <$> (empty *> many pt <* token "=") <*> d.exp d))
      in
      keyword "let" *> empty_lr (elet <$> (many binding <* keyword "in") <*> d.exp d)
      <* keyword "end"
    in
    let efun = empty_lr (efun <$> (keyword "fn" *> many pt <* token "=>") <*> d.exp d) in
    let ematch =
      let fst_case = ccase <$> (option "" (token "|") *> pt <* token "=>") <*> d.exp d in
      let other_cases = ccase <$> (token "|" *> pt <* token "=>") <*> d.exp d in
      let cases = (fun fst other -> fst :: other) <$> fst_case <*> many other_cases in
      let fncases = (fun fst other -> fst :: other) <$> fst_case <*> many1 other_cases in
      let pmatch = ematch <$> (keyword "case" *> d.exp d <* keyword "of") <*> cases in
      let pfunction = keyword "fn" *> fncases >>| efunction in
      empty_lr @@ pfunction <|> pmatch
    in
    choice [ elet; eif; ematch; efun ]
  in
  let tuple d =
    ( @ )
    <$> token "(" *> many1 (d.exp d <* token ",")
    <*> (d.op d <|> d.key d >>| fun rhs -> [ rhs ])
    <* token ")"
    >>| etuple
  in
  let op d =
    fix
    @@ fun _self ->
    let lst =
      empty_lr @@ between (token "[") (token "]") @@ sep_by (token ",") (d.exp d)
    in
    let none = token "NONE" *> return ENone in
    let prim =
      empty_lr
      @@ choice
           [ lst >>| elist
           ; none
           ; uns_const >>| econst
           ; ident >>| evar
           ; tuple d
           ; brackets @@ d.exp d
           ]
    in
    let some = token "SOME" *> prim >>= fun x -> return @@ ESome x in
    let app_op = empty_lr @@ chainl1' prim prim eapp in
    let app_or_some = app_op <|> some in
    let mul_op = procl (choice_op [ "*", Mul; "div", Div ]) app_or_some @@ d.key d in
    let add_op =
      procl
        (choice_op [ "+", Add; "-", Sub ])
        (apply_unary mul_op)
        (apply_unary @@ d.key d)
    in
    let cons_op = procr cons add_op @@ d.key d in
    let cmp_op =
      procl (choice_op [ ">=", Geq; ">", Gre; "<=", Leq; "<", Less ]) cons_op @@ d.key d
    in
    let eq_op = procl (choice_op [ "=", Eq; "<>", Neq ]) cmp_op @@ d.key d in
    let conj_op = procl (choice_op [ "andalso", And ]) eq_op @@ d.key d in
    let disj_op = procl (choice_op [ "orelse", Or ]) conj_op @@ d.key d in
    empty_lr @@ disj_op
  in
  { key; op; exp; tuple }
;;

let decl =
  lift3
    dlet
    (keyword "val" *> option false (keyword "rec" >>| fun _ -> true))
    pt
    (empty *> token "=" *> pack.exp pack)
  <|> lift3
        dlet
        (keyword "fun" *> return true)
        pt
        (efun <$> (empty *> many pt <* token "=") <*> pack.exp pack)
;;

let pprog (l : decl list) : prog = l
let prog = sep_by1 (token ";;") decl <* option "" @@ empty_lr (token ";;") >>| pprog
