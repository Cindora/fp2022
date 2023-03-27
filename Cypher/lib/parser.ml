(** Copyright 2022-2023, Drumov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

let parse_string p s = Angstrom.parse_string ~consume:Consume.All p s

let is_wspace =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let keywrds_list =
  [ "CREATE"
  ; "MATCH"
  ; "START"
  ; "ORDER"
  ; "ASC"
  ; "DESC"
  ; "AND"
  ; "IN"
  ; "NOT"
  ; "NULL"
  ; "XOR"
  ; "OR"
  ; "AS"
  ; "WHEN"
  ; "THEN"
  ; "ELSE"
  ; "WITH"
  ; "DELETE"
  ; "WHERE"
  ; "SET"
  ; "DETACH"
  ; "RETURN"
  ]
;;

let wspaces_p p = is_wspace *> p <* is_wspace
let wspaces_char ch = wspaces_p @@ char ch
let wspaces_str str = wspaces_p @@ string_ci str
let parens p = wspaces_char '(' *> p <* wspaces_char ')'
let braces p = wspaces_char '{' *> p <* wspaces_char '}'
let sq_brackets p = wspaces_char '[' *> p <* wspaces_char ']'
let colon p = wspaces_char ':' *> p

let variables_p =
  wspaces_p @@ take_while1 (fun ch -> is_letter ch || is_digit ch || ch = '_')
  >>= fun v ->
  if List.mem v keywrds_list then fail "Variables cannot be keyword" else return v
;;

let labelbackticks = wspaces_char '`' *> take_while (( != ) '`') <* wspaces_char '`'
let cint v = CInt v
let cstr str = CString str

let dquotes_cstr_p =
  cstr <$> wspaces_char '\"' *> take_while1 (( != ) '\"') <* wspaces_char '\"'
;;

let squotes_cstr_p =
  cstr <$> wspaces_char '\'' *> take_while1 (( != ) '\'') <* wspaces_char '\''
;;

let sign_p =
  choice
    [ wspaces_char '-' *> return (-1)
    ; wspaces_char '+' *> return 1
    ; wspaces_str "" *> return 1
    ]
;;

let cint_p =
  wspaces_p
  @@ lift2 (fun sign num -> cint (int_of_string num * sign)) sign_p (take_while1 is_digit)
;;

let const_p = dquotes_cstr_p <|> squotes_cstr_p <|> cint_p
let e_const = (fun c -> EConst c) <$> const_p

let e_get_prop =
  lift2
    (fun var prop -> EGetProp (var, prop))
    (variables_p <* wspaces_char '.')
    variables_p
;;

let e_get_type = (fun var -> EGetType var) <$> wspaces_str "type" *> parens variables_p
let e_get_alias = (fun var -> EGetAlias var) <$> variables_p
let e_get_elm = (fun var -> EGetElm var) <$> variables_p
let null_p = (fun _ -> IsNull) <$> wspaces_str "NULL"
let not_null_p = (fun _ -> IsNotNull) <$> wspaces_str "NOT" *> wspaces_str "NULL"

let e_null =
  lift2
    (fun prop op -> EUnop (op, prop))
    e_get_prop
    (wspaces_str "IS" *> (not_null_p <|> null_p))
;;

let e_p =
  let helper p op = p *> return (fun e1 e2 -> EBinop (op, e1, e2)) in
  let mul_p = helper (char '*') Mul in
  let div_p = helper (char '/') Div in
  let add_p = helper (char '+') Add in
  let sub_p = helper (char '-') Sub in
  let eq_p = helper (char '=') Eq in
  let neq_p = helper (string "<>") NEq in
  let less_p = helper (char '<') Less in
  let gre_p = helper (char '>') Gre in
  let leq_p = helper (string "<=") LEq in
  let geq_p = helper (string ">=") GEq in
  let and_p = helper (string_ci "&&") And in
  let or_p = helper (string_ci "||") Or in
  let kw_and_p = helper (string_ci "AND ") KWAnd in
  let kw_or_p = helper (string_ci "OR ") KWOr in
  let kw_xor_p = helper (string_ci "XOR ") Xor in
  let e_as =
    lift2
      (fun e1 e2 -> EBinop (As, e1, e2))
      (e_get_prop <|> e_get_elm)
      (string_ci "AS " *> e_get_alias)
  in
  fix
  @@ fun e_p ->
  let kw_not_p = (fun e -> EUnop (KWNot, e)) <$> wspaces_str "NOT " *> e_p in
  let not_p = (fun e -> EUnop (Not, e)) <$> wspaces_str "!" *> e_p in
  let chainl1 e op =
    let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
    e >>= fun init -> go init
  in
  let e_not = kw_not_p <|> not_p in
  let choice_p =
    e_const
    <|> parens e_p
    <|> e_as
    <|> e_get_type
    <|> e_not
    <|> e_null
    <|> e_get_prop
    <|> e_get_elm
  in
  let muldiv_op = chainl1 choice_p (mul_p <|> div_p) in
  let addsub_op = chainl1 muldiv_op (add_p <|> sub_p) in
  let compare_op =
    chainl1 addsub_op (neq_p <|> leq_p <|> geq_p <|> less_p <|> gre_p <|> eq_p)
  in
  let and_op = chainl1 compare_op and_p in
  let or_op = chainl1 and_op or_p in
  let kw_and_op = chainl1 or_op kw_and_p in
  let kw_or_op = chainl1 kw_and_op kw_or_p in
  let xor_op = chainl1 kw_or_op kw_xor_p in
  xor_op
;;

let property_p =
  lift2
    (fun str expr -> str, expr)
    (take_till (fun ch -> ch = ':' || ch = ' ') <* wspaces_char ':')
    e_p
;;

let properties_p = sep_by (wspaces_char ',') property_p

let property =
  braces
    ((fun props ->
       match props with
       | [] -> None
       | _ -> Some props)
    <$> properties_p)
;;

let node_data =
  parens
  @@ lift3
       (fun var label props -> NodeData (var, label, props))
       (option None ((fun var -> Some var) <$> variables_p))
       (option
          None
          (colon
             ((fun vars ->
                match vars with
                | [] -> None
                | _ -> Some vars)
             <$> sep_by (wspaces_char ':') variables_p)))
       (option None property)
;;

let edge_data =
  lift3
    (fun var label props -> var, label, props)
    (option None ((fun var -> Some var) <$> variables_p))
    (option None (colon ((fun var -> Some var) <$> (labelbackticks <|> variables_p))))
    (option None property)
;;

let edge_direction_r =
  wspaces_char '[' *> edge_data
  >>= (function
        | var, label, props ->
          wspaces_char ']'
          *> wspaces_char '-'
          *> option
               (EdgeData (UnDirect (var, label, props)))
               ((fun _ -> EdgeData (DirectR (var, label, props))) <$> wspaces_char '>'))
  <|> wspaces_char '-'
      *> option
           (EdgeData (UnDirect (None, None, None)))
           ((fun _ -> EdgeData (DirectR (None, None, None))) <$> wspaces_char '>')
;;

let edge_direction_l =
  wspaces_char '-' *> wspaces_char '[' *> edge_data
  >>= (function
        | var, label, props ->
          wspaces_char ']'
          *> ((fun _ -> EdgeData (DirectL (var, label, props))) <$> wspaces_char '-'))
  <|> wspaces_char '-'
      *> ((fun _ -> EdgeData (DirectL (None, None, None))) <$> wspaces_char '-')
;;

let edge_dir =
  wspaces_char '-' *> edge_direction_r <|> wspaces_char '<' *> edge_direction_l
;;

let edge_p = lift3 (fun n1 e n2 -> Edge (n1, e, n2)) node_data edge_dir node_data
let node_p = lift (fun node -> Node node) node_data
let elm_p = edge_p <|> node_p

let enum_p = function
  | p -> sep_by (wspaces_char ',') p
;;

let create_p = wspaces_str "CREATE" *> lift (fun cmd -> CmdCreate cmd) (enum_p elm_p)
let m_create_p = wspaces_str "CREATE" *> lift (fun cmd -> CMCreate cmd) (enum_p elm_p)
let m_return_p = wspaces_str "RETURN" *> lift (fun expr -> CMReturn expr) (enum_p e_p)

let m_delete_p =
  wspaces_str "DELETE" *> lift (fun var -> CMDelete var) (enum_p variables_p)
;;

let m_det_delete_p =
  wspaces_str "DETACH"
  *> wspaces_str "DELETE"
  *> lift (fun var -> CMDetachDelete var) (enum_p variables_p)
;;

(* let m_set_p =
  wspaces_str "SET"
  *> lift3
       (fun e1 e2 props -> CMSet (e1, e2, props))
       (e_get_prop <|> e_get_var <* wspaces_char '=')
       (option None ((fun var -> Some var) <$> e_const))
       (option None property)
;; *)

let m_where_p = wspaces_str "WHERE" *> lift (fun expr -> CMWhere expr) e_p
let match_with_cmd = m_where_p (* <|> m_set_p *)
let match_cmd = m_create_p <|> m_delete_p <|> m_det_delete_p <|> m_return_p

let cmd_match_p =
  lift3
    (fun elm match_with_cmd match_cmd -> CmdMatch (elm, match_with_cmd, match_cmd))
    (enum_p elm_p)
    (option None ((fun cmd -> Some cmd) <$> match_with_cmd))
    (many1 match_cmd)
;;

let match_p = wspaces_str "MATCH" *> cmd_match_p
let cmds_p = create_p <|> match_p
let statements_p = sep_by1 (wspaces_char ';') cmds_p
let parse program = parse_string statements_p program

let interprete_parse f p str =
  match parse_string p str with
  | Result.Error e -> Format.printf "Error: %s" e
  | Result.Ok ast -> Format.printf "%s" (f ast)
;;

let%expect_test _ =
  interprete_parse show_constant const_p "123";
  [%expect {| (CInt 123) |}]
;;

let%expect_test _ =
  interprete_parse show_constant const_p "\"Hey\"";
  [%expect {| (CString "Hey") |}]
;;

let%expect_test _ =
  interprete_parse show_constant const_p "\'How, are you?\'";
  [%expect {| (CString "How, are you?") |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "0 + 1 / a";
  [%expect
    {|
    (EBinop (Add, (EConst (CInt 0)),
       (EBinop (Div, (EConst (CInt 1)), (EGetElm "a"))))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "(1 * b - 2)";
  [%expect
    {|
    (EBinop (Sub, (EBinop (Mul, (EConst (CInt 1)), (EGetElm "b"))),
       (EConst (CInt 2)))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "((3 + 5 - c > d))";
  [%expect
    {|
    (EBinop (Gre,
       (EBinop (Sub, (EBinop (Add, (EConst (CInt 3)), (EConst (CInt 5)))),
          (EGetElm "c"))),
       (EGetElm "d"))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "((8 + 13) - e) <> 21";
  [%expect
    {|
    (EBinop (NEq,
       (EBinop (Sub, (EBinop (Add, (EConst (CInt 8)), (EConst (CInt 13)))),
          (EGetElm "e"))),
       (EConst (CInt 21)))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "n.name AS name AND n.age < f";
  [%expect
    {|
    (EBinop (KWAnd, (EBinop (As, (EGetProp ("n", "name")), (EGetAlias "name"))),
       (EBinop (Less, (EGetProp ("n", "age")), (EGetElm "f"))))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "n && n.age > f";
  [%expect
    {|
    (EBinop (And, (EGetElm "n"),
       (EBinop (Gre, (EGetProp ("n", "age")), (EGetElm "f"))))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "n.name = \"Alex\" OR n.age <= 20";
  [%expect
    {|
    (EBinop (KWOr,
       (EBinop (Eq, (EGetProp ("n", "name")), (EConst (CString "Alex")))),
       (EBinop (LEq, (EGetProp ("n", "age")), (EConst (CInt 20)))))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "n.name || n.age >= 15";
  [%expect
    {|
    (EBinop (Or, (EGetProp ("n", "name")),
       (EBinop (GEq, (EGetProp ("n", "age")), (EConst (CInt 15)))))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "a XOR b";
  [%expect {| (EBinop (Xor, (EGetElm "a"), (EGetElm "b"))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "NOT a";
  [%expect {| (EUnop (KWNot, (EGetElm "a"))) |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "type(n)";
  [%expect {| (EGetType "n") |}]
;;

let%expect_test _ =
  interprete_parse show_expr e_p "n.name IS NULL";
  [%expect {| (EUnop (IsNull, (EGetProp ("n", "name")))) |}]
;;

(* let%expect_test _ =
  interprete_parse show_expr e_p "!%&%";
  [%expect {| (EUnop (Not, (EBinop (And, EWildcard, EWildcard)))) |}]
;; *)

let%expect_test _ =
  interprete_parse show_property property_p "age:22";
  [%expect {| ("age", (EConst (CInt 22))) |}]
;;

let%expect_test _ =
  interprete_parse show_elm node_p "(n:Person {name:\"Eliot\"})";
  [%expect
    {|
    (Node
       (NodeData ((Some "n"), (Some ["Person"]),
          (Some [("name", (EConst (CString "Eliot")))])))) |}]
;;

let%expect_test _ =
  interprete_parse
    show_elm
    edge_p
    "(n:Person1 {name:\"Eliot\"})<-[:DIRECTED]-(k:Person2 {name:\"Tyler\"})";
  [%expect
    {|
    (Edge (
       (NodeData ((Some "n"), (Some ["Person1"]),
          (Some [("name", (EConst (CString "Eliot")))]))),
       (EdgeData (DirectL (None, (Some "DIRECTED"), None))),
       (NodeData ((Some "k"), (Some ["Person2"]),
          (Some [("name", (EConst (CString "Tyler")))])))
       )) |}]
;;

let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "CREATE \n\
    \    (charlie:Person {name:\'Charlie Sheen\'}),\n\
    \    (martin:Person {name:\'Martin Sheen\'}),\n\
    \    (michael:Person {name:\'Michael Douglas\'}),\n\
    \    (oliver:Person {name:\'Oliver Stone\'}),\n\
    \    (rob:Person {name:\'Rob Reiner\'}),\n\
    \    (wallStreet:Movie {title:\'Wall Street\'}),\n\
    \    (charlie)-[:ACTED_IN {role:\'Bud Fox\'}]->(wallStreet),\n\
    \    (martin)-[:ACTED_IN {role:\'Carl Fox\'}]->(wallStreet),\n\
    \    (michael)-[:ACTED_IN {role:\'Gordon Gekko\'}]->(wallStreet),\n\
    \    (oliver)-[:DIRECTED]->(wallStreet),\n\
    \    (thePresident:Movie {title:\'The American President\'}),\n\
    \    (martin)-[:ACTED_IN {role:\'A.J. MacInerney\'}]->(thePresident),\n\
    \    (michael)-[:ACTED_IN {role:\'President Andrew Shepherd\'}]->(thePresident),\n\
    \    (rob)-[:DIRECTED]->(thePresident),\n\
    \    (martin)-[:FATHER_OF]->(charlie)";
  [%expect
    {|
    (CmdCreate
       [(Node
           (NodeData ((Some "charlie"), (Some ["Person"]),
              (Some [("name", (EConst (CString "Charlie Sheen")))]))));
         (Node
            (NodeData ((Some "martin"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Martin Sheen")))]))));
         (Node
            (NodeData ((Some "michael"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Michael Douglas")))]))));
         (Node
            (NodeData ((Some "oliver"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Oliver Stone")))]))));
         (Node
            (NodeData ((Some "rob"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Rob Reiner")))]))));
         (Node
            (NodeData ((Some "wallStreet"), (Some ["Movie"]),
               (Some [("title", (EConst (CString "Wall Street")))]))));
         (Edge ((NodeData ((Some "charlie"), None, None)),
            (EdgeData
               (DirectR
                  (None, (Some "ACTED_IN"),
                   (Some [("role", (EConst (CString "Bud Fox")))])))),
            (NodeData ((Some "wallStreet"), None, None))));
         (Edge ((NodeData ((Some "martin"), None, None)),
            (EdgeData
               (DirectR
                  (None, (Some "ACTED_IN"),
                   (Some [("role", (EConst (CString "Carl Fox")))])))),
            (NodeData ((Some "wallStreet"), None, None))));
         (Edge ((NodeData ((Some "michael"), None, None)),
            (EdgeData
               (DirectR
                  (None, (Some "ACTED_IN"),
                   (Some [("role", (EConst (CString "Gordon Gekko")))])))),
            (NodeData ((Some "wallStreet"), None, None))));
         (Edge ((NodeData ((Some "oliver"), None, None)),
            (EdgeData (DirectR (None, (Some "DIRECTED"), None))),
            (NodeData ((Some "wallStreet"), None, None))));
         (Node
            (NodeData ((Some "thePresident"), (Some ["Movie"]),
               (Some [("title", (EConst (CString "The American President")))]))));
         (Edge ((NodeData ((Some "martin"), None, None)),
            (EdgeData
               (DirectR
                  (None, (Some "ACTED_IN"),
                   (Some [("role", (EConst (CString "A.J. MacInerney")))])))),
            (NodeData ((Some "thePresident"), None, None))));
         (Edge ((NodeData ((Some "michael"), None, None)),
            (EdgeData
               (DirectR
                  (None, (Some "ACTED_IN"),
                   (Some [("role", (EConst (CString "President Andrew Shepherd")))
                           ])))),
            (NodeData ((Some "thePresident"), None, None))));
         (Edge ((NodeData ((Some "rob"), None, None)),
            (EdgeData (DirectR (None, (Some "DIRECTED"), None))),
            (NodeData ((Some "thePresident"), None, None))));
         (Edge ((NodeData ((Some "martin"), None, None)),
            (EdgeData (DirectR (None, (Some "FATHER_OF"), None))),
            (NodeData ((Some "charlie"), None, None))))
         ]) |}]
;;

let%expect_test _ =
  interprete_parse show_command cmds_p "MATCH (movie:Movie)\nRETURN movie.title";
  [%expect
    {|
    (CmdMatch ([(Node (NodeData ((Some "movie"), (Some ["Movie"]), None)))],
       None, [(CMReturn [(EGetProp ("movie", "title"))])])) |}]
;;

let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "MATCH (n:Person)\nRETURN n.name AS SomethingTotallyDifferent";
  [%expect
    {|
    (CmdMatch ([(Node (NodeData ((Some "n"), (Some ["Person"]), None)))], None,
       [(CMReturn
           [(EBinop (As, (EGetProp ("n", "name")),
               (EGetAlias "SomethingTotallyDifferent")))
             ])
         ]
       )) |}]
;;

let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "MATCH\n\
    \  (person:Person),\n\
    \  (peter:Person {name: \'Peter\'})\n\
     RETURN person.name, person.age";
  [%expect
    {|
    (CmdMatch (
       [(Node (NodeData ((Some "person"), (Some ["Person"]), None)));
         (Node
            (NodeData ((Some "peter"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Peter")))]))))
         ],
       None,
       [(CMReturn [(EGetProp ("person", "name")); (EGetProp ("person", "age"))])]
       )) |}]
;;

(* let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "MATCH (n {name: \'Andy\'})\n\
    \    SET n.surname = \'Taylor\'\n\
    \    RETURN n.name, n.surname";
  [%expect
    {|
    (CmdMatch (
       [(Node
           (NodeData ((Some "n"), None,
              (Some [(Props ("name", (EConst (CString "Andy"))))]))))
         ],
       (Some (CMSet ((EGetProp ("n", "surname")),
                (Some (EConst (CString "Taylor"))), None))),
       [(CMReturn [(EGetProp ("n", "name")); (EGetProp ("n", "surname"))])])) |}]
;; *)

let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "MATCH\n\
    \    (charlie:Person {name: \'Martin Sheen\'}),\n\
    \    (rob:Person {name: \'Rob Reiner\'})\n\
    \  CREATE (rob)-[:OLD_FRIENDS]->(martin)";
  [%expect
    {|
    (CmdMatch (
       [(Node
           (NodeData ((Some "charlie"), (Some ["Person"]),
              (Some [("name", (EConst (CString "Martin Sheen")))]))));
         (Node
            (NodeData ((Some "rob"), (Some ["Person"]),
               (Some [("name", (EConst (CString "Rob Reiner")))]))))
         ],
       None,
       [(CMCreate
           [(Edge ((NodeData ((Some "rob"), None, None)),
               (EdgeData (DirectR (None, (Some "OLD_FRIENDS"), None))),
               (NodeData ((Some "martin"), None, None))))
             ])
         ]
       )) |}]
;;

let%expect_test _ =
  interprete_parse
    show_command
    cmds_p
    "MATCH (charlie {name: \'Charlie Sheen\'})-[:ACTED_IN]->(movie),\n\n\
    \     (movie)<-[:DIRECTED]-(director)\n\
     WHERE director.name = \" Rob Wheelson \"\n\
    \        RETURN movie.title, director.name";
  [%expect
    {|
    (CmdMatch (
       [(Edge (
           (NodeData ((Some "charlie"), None,
              (Some [("name", (EConst (CString "Charlie Sheen")))]))),
           (EdgeData (DirectR (None, (Some "ACTED_IN"), None))),
           (NodeData ((Some "movie"), None, None))));
         (Edge ((NodeData ((Some "movie"), None, None)),
            (EdgeData (DirectL (None, (Some "DIRECTED"), None))),
            (NodeData ((Some "director"), None, None))))
         ],
       (Some (CMWhere
                (EBinop (Eq, (EGetProp ("director", "name")),
                   (EConst (CString "Rob Wheelson ")))))),
       [(CMReturn
           [(EGetProp ("movie", "title")); (EGetProp ("director", "name"))])
         ]
       )) |}]
;;
