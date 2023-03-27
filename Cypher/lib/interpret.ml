(** Copyright 2022-2023, Drumov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Graphlib.Std
open Graph
open Base

type error =
  | IncorrectType
  | IncorrectProps
  | IncorrectCreate of string
  | IncorrectWhere of string
  | DivisionByZero
  | UnboundValue of string
  | UnstableElm
  | ElmNotValid
  | TypeNotValid of string
[@@deriving show { with_path = false }]

type values =
  | VString of string
  | VInt of int
  | VBool of bool
  | VNull of string
[@@deriving show { with_path = false }]

type vlabels = string list [@@deriving show { with_path = false }]
type vprops = (string * values) list [@@deriving show { with_path = false }]

(** src and dst are the node types of the source and destination edges:
    (label, property) 
    labels - similar to tags and allow you to specify certain types of entities to search for or create,
    properties - name-value pairs that provide additional details to our nodes and relationships.*)
type node_src_dst = vlabels * vprops [@@deriving show { with_path = false }]

(** Edges and Nodes have the same type.
Node: (label, property), (None, None)
Edge: (label, property), (Some src, Some dst) *)
type elm = (vlabels * vprops) * (node_src_dst option * node_src_dst option)
[@@deriving show { with_path = false }]

type value =
  | Value of values
  | VElm of elm
[@@deriving show { with_path = false }]

let pp_value fmt value =
  let open Stdlib.Format in
  match value with
  | VString s -> fprintf fmt "%S" s
  | VInt n -> fprintf fmt "%d" n
  | VBool b -> fprintf fmt "%b" b
  | VNull s -> fprintf fmt "%s" s
;;

let pp_props fmt labels =
  let open Stdlib.Format in
  let pp_label fmt = function
    | str, value -> fprintf fmt "%S: %a" str pp_value value
  in
  fprintf
    fmt
    "\n{  %a  }"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",\n") pp_label)
    labels
;;

let pp_type fmt labels =
  let open Stdlib.Format in
  let pp_type fmt label = fprintf fmt "%S" label in
  fprintf
    fmt
    "\n[  %a  ],"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",\n") pp_type)
    labels
;;

let pp_labels fmt labels =
  let open Stdlib.Format in
  let pp_label fmt label = fprintf fmt "%S" label in
  fprintf
    fmt
    "\n{  %a  },"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",\n") pp_label)
    labels
;;

let cval v = Value v
let cvstr str = VString str
let cvint i = VInt i
let cvbool b = VBool b
let cvnull n = VNull n
let cvelm e = VElm e

module MElm = struct
  type t = elm

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = Stdlib.( = )
  let default : t = ([], []), (None, None)
end

module LGraph = Graphlib.Of_ocamlgraph (Persistent.Digraph.ConcreteLabeled (MElm) (MElm))

type graph = LGraph.t
type env = graph * (string, elm list) Map.Poly.t

type elms =
  | Node_elm
  | Edge_elm

let ( let* ) m f = Stdlib.Result.bind m f
let return = Result.return
let fail = Result.fail
let empty = LGraph.empty, Map.Poly.empty
let extend_by_one var elm map = Map.Poly.add_multi map ~key:var ~data:elm

let find env var =
  let list_elm = Map.Poly.find_multi env var in
  let rec helper = function
    | [] -> None
    | h :: tl ->
      (match h with
       | (_, _), (None, None) -> Some h
       | _ -> helper tl)
  in
  helper list_elm
;;

let length map =
  List.fold_left
    ~f:(fun acc lst ->
      let* acc = acc in
      return (acc + List.length lst))
    ~init:(return 0)
    (Map.Poly.data map)
;;

let find_value_props var var_props env =
  let* list_elm = return @@ Map.Poly.find_multi env var in
  if List.is_empty list_elm
  then fail @@ UnboundValue "Undefined variable or nothing was found."
  else
    List.fold_left
      ~f:(fun acc elm ->
        let* valuelist = acc in
        match elm with
        | (_, props), (_, _) ->
          (match Stdlib.List.assoc_opt var_props props with
           | Some value -> return (cval value :: valuelist)
           | None -> return (cval (cvnull "null") :: valuelist)))
      ~init:(return [])
      list_elm
;;

let find_type var env =
  let list_elm = Map.Poly.find_multi env var in
  if List.is_empty list_elm
  then fail @@ UnboundValue "Undefined variable or nothing was found."
  else
    List.fold_left
      ~f:(fun acc elm ->
        let* labellist = acc in
        match elm with
        | (label, _), (Some _, Some _) ->
          (match label with
           | lbl :: _ -> return (cval (cvstr lbl) :: labellist)
           | _ -> return labellist)
        | (_, _), (None, None) ->
          fail
          @@ TypeNotValid "Type is only available at the edges. Nodes are not supported."
        | _ -> return labellist)
      ~init:(return [])
      list_elm
;;

let find_elm var env =
  let list_elm = Map.Poly.find_multi env var in
  if List.is_empty list_elm
  then fail @@ UnboundValue "Undefined variable or nothing was found."
  else
    List.fold_left
      ~f:(fun acc elm ->
        let* valuelist = acc in
        match elm with
        | elm -> return (cvelm elm :: valuelist))
      ~init:(return [])
      list_elm
;;

let check_props var var_props mvar mprops =
  if Poly.( = ) var mvar
  then (
    match List.Assoc.find mprops ~equal:Poly.( = ) var_props with
    | Some value -> return value
    | None -> return @@ cvnull "null")
  else fail UnstableElm
;;

let rec interpret_expr_for_ret = function
  | EConst (CString str) -> return str
  | EConst (CInt i) -> return @@ Int.to_string i
  | EGetProp (var, var_props) -> return @@ var ^ "." ^ var_props
  | EGetType r -> return @@ "type(" ^ r ^ ")"
  | EGetAlias var -> return var
  | EGetElm var -> return var
  | EBinop (op, e1, e2) ->
    let* v1 = interpret_expr_for_ret e1 in
    let* v2 = interpret_expr_for_ret e2 in
    let ret op = return @@ v1 ^ op ^ v2 in
    (match op with
     | Add -> ret "+"
     | Sub -> ret "-"
     | Mul -> ret "*"
     | Div -> ret "/"
     | Eq -> ret "="
     | NEq -> ret "<>"
     | Less -> ret "<"
     | Gre -> ret ">"
     | LEq -> ret "<="
     | GEq -> ret ">="
     | And -> ret " && "
     | Or -> ret " || "
     | KWAnd -> ret " AND "
     | KWOr -> ret " OR  "
     | Xor -> ret " XOR "
     | As -> return v2)
  | EUnop (op, expr) ->
    let* elm = interpret_expr_for_ret expr in
    (match op with
     | Not -> return @@ " NOT " ^ elm
     | KWNot -> return @@ " ! " ^ elm
     | IsNull -> return @@ elm ^ " IS NULL "
     | IsNotNull -> return @@ elm ^ " IS NOT NULL")
;;

let rec interpret_expr_where_r expr mvar mprops =
  match expr with
  | EConst (CString str) -> return @@ cvstr str
  | EConst (CInt i) -> return @@ cvint i
  | EGetProp (var, var_props) -> check_props var var_props mvar mprops
  | EGetElm _ -> fail ElmNotValid
  | EGetAlias _ -> fail @@ IncorrectWhere "The As operation is not supported in WHERE"
  | EGetType _ -> fail @@ IncorrectWhere "The query should look like this ()-[r: TYPE]-()"
  | EBinop (op, e1, e2) ->
    let* v1 = interpret_expr_where_r e1 mvar mprops in
    let* v2 = interpret_expr_where_r e2 mvar mprops in
    (match op, v1, v2 with
     | Add, VInt v1, VInt v2 -> return @@ cvint (v1 + v2)
     | Sub, VInt v1, VInt v2 -> return @@ cvint (v1 - v2)
     | Mul, VInt v1, VInt v2 -> return @@ cvint (v1 * v2)
     | Div, VInt _, VInt v2 when v2 = 0 -> fail DivisionByZero
     | Div, VInt v1, VInt v2 -> return @@ cvint @@ (v1 / v2)
     | Eq, _, _ -> return @@ cvbool @@ Poly.( = ) v1 v2
     | NEq, _, _ -> return @@ cvbool @@ Poly.( <> ) v1 v2
     | Less, _, _ -> return @@ cvbool @@ Poly.( < ) v1 v2
     | Gre, _, _ -> return @@ cvbool @@ Poly.( > ) v1 v2
     | LEq, _, _ -> return @@ cvbool @@ Poly.( <= ) v1 v2
     | GEq, _, _ -> return @@ cvbool @@ Poly.( >= ) v1 v2
     | KWAnd, VBool v1, VBool v2 | And, VBool v1, VBool v2 -> return @@ cvbool (v1 && v2)
     | KWOr, VBool v1, VBool v2 | Or, VBool v1, VBool v2 -> return @@ cvbool (v1 || v2)
     | Xor, VBool v1, VBool v2 -> return @@ cvbool @@ Poly.( <> ) v1 v2
     | _ -> fail IncorrectType)
  | EUnop (op, expr) ->
    let* expr = interpret_expr_where_r expr mvar mprops in
    (match op, expr with
     | KWNot, VBool expr | Not, VBool expr -> return @@ cvbool @@ not expr
     | IsNotNull, VNull _ -> return @@ cvbool false
     | IsNotNull, _ -> return @@ cvbool true
     | IsNull, VNull _ -> return @@ cvbool true
     | IsNull, _ -> return @@ cvbool false
     | _ -> fail IncorrectType)
;;

let interpret_expr_where expr mvar mprops =
  match expr with
  | EBinop (op, e1, e2) ->
    let* v1 = interpret_expr_where_r e1 mvar mprops in
    let* v2 = interpret_expr_where_r e2 mvar mprops in
    (match op with
     | Eq -> return @@ Poly.( = ) v1 v2
     | NEq -> return @@ Poly.( <> ) v1 v2
     | Less -> return @@ Poly.( < ) v1 v2
     | Gre -> return @@ Poly.( > ) v1 v2
     | LEq -> return @@ Poly.( <= ) v1 v2
     | GEq -> return @@ Poly.( >= ) v1 v2
     | KWAnd | And ->
       (match v1, v2 with
        | VBool v1, VBool v2 -> return (v1 && v2)
        | _, _ -> fail IncorrectType)
     | KWOr | Or ->
       (match v1, v2 with
        | VBool v1, VBool v2 -> return (v1 || v2)
        | _, _ -> fail IncorrectType)
     | Xor ->
       (match v1, v2 with
        | VBool v1, VBool v2 -> return @@ Poly.( <> ) v1 v2
        | _, _ -> fail IncorrectType)
     | _ -> fail IncorrectType)
  | EUnop (op, expr) ->
    let* expr = interpret_expr_where_r expr mvar mprops in
    (match op, expr with
     | KWNot, VBool expr | Not, VBool expr -> return @@ not expr
     | IsNotNull, VNull _ -> return false
     | IsNotNull, _ -> return true
     | IsNull, VNull _ -> return true
     | IsNull, _ -> return false
     | _ -> fail IncorrectType)
  | _ -> fail IncorrectType
;;

let rec interpret_expr env = function
  | EConst (CString s) -> return [ cval @@ cvstr s ]
  | EConst (CInt i) -> return [ cval @@ cvint i ]
  | EGetProp (var, var_props) -> find_value_props var var_props env
  | EGetType r -> find_type r env
  | EGetElm var -> find_elm var env
  | EGetAlias str -> return [ cval @@ cvstr str ]
  | EBinop (op, e1, e2) ->
    let* vlist1 = interpret_expr env e1 in
    let* vlist2 = interpret_expr env e2 in
    List.fold_left
      ~f:(fun acc v1 ->
        let* valuelist = acc in
        List.fold_left
          ~f:(fun acc v2 ->
            let* valuelist = acc in
            let ret value = return (cval value :: valuelist) in
            match v1, v2 with
            | Value v1, Value v2 ->
              (match op, v1, v2 with
               | Add, VInt v1, VInt v2 -> ret @@ cvint (v1 + v2)
               | Sub, VInt v1, VInt v2 -> ret @@ cvint (v1 - v2)
               | Mul, VInt v1, VInt v2 -> ret @@ cvint (v1 * v2)
               | Div, VInt _, VInt v2 when v2 = 0 -> fail DivisionByZero
               | Div, VInt v1, VInt v2 -> ret @@ cvint (v1 / v2)
               | Eq, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( = ) v1 v2
               | NEq, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( <> ) v1 v2
               | Less, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( < ) v1 v2
               | Gre, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( > ) v1 v2
               | LEq, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( <= ) v1 v2
               | GEq, VInt v1, VInt v2 -> ret @@ cvbool @@ Poly.( >= ) v1 v2
               | KWAnd, VBool v1, VBool v2 | And, VBool v1, VBool v2 ->
                 ret @@ cvbool (v1 && v2)
               | KWOr, VBool v1, VBool v2 | Or, VBool v1, VBool v2 ->
                 ret @@ cvbool (v1 || v2)
               | Xor, VBool v1, VBool v2 -> ret @@ cvbool @@ Poly.( <> ) v1 v2
               | As, value, _ -> return (cval value :: valuelist)
               | _ -> fail IncorrectType)
            | value, _ ->
              (match op, value with
               | As, value -> return (value :: valuelist)
               | _ -> fail IncorrectType))
          ~init:(return valuelist)
          vlist2)
      ~init:(return [])
      vlist1
  | EUnop (op, expr) ->
    let* vlist = interpret_expr env expr in
    List.fold_left
      ~f:(fun acc v ->
        let* valuelist = acc in
        let ret value = return (cval value :: valuelist) in
        match op, v with
        | Not, Value (VBool v) | KWNot, Value (VBool v) -> ret @@ cvbool @@ not v
        | IsNull, Value (VNull _) -> ret @@ cvnull "null"
        | IsNull, _ -> return valuelist
        | IsNotNull, Value (VNull _) -> return valuelist
        | IsNotNull, Value v -> ret v
        | _ -> fail IncorrectType)
      ~init:(return [])
      vlist
;;

let get_props env props =
  List.fold_left
    ~f:(fun acc (var, v) ->
      let* valuelist = interpret_expr env v in
      match valuelist with
      | [] -> fail IncorrectProps
      | h :: _ ->
        (match h with
         | Value value ->
           let* lst = acc in
           return ((var, value) :: lst)
         | VElm _ -> fail IncorrectProps))
    ~init:(return [])
    props
;;

let save_node g env var vlabels vprops =
  let node = LGraph.Node.create ((vlabels, vprops), (None, None)) in
  let graph = LGraph.Node.insert node g in
  let env =
    match var with
    | None -> env
    | Some var -> extend_by_one var node env
  in
  return (graph, env, node)
;;

let add_node g env var label props =
  match label, props with
  | Some label, Some props ->
    let* vprops = get_props env props in
    save_node g env var label vprops
  | None, Some props ->
    let* vprops = get_props env props in
    save_node g env var [] vprops
  | Some label, None -> save_node g env var label []
  | None, None -> save_node g env var [] []
;;

let check_node g env = function
  | NodeData (var, label, props) ->
    (match var with
     | Some var ->
       (match find env var with
        | None ->
          (match add_node g env (Option.some var) label props with
           | Ok (g, env, node) -> return (g, env, node)
           | Error error -> fail error)
        | Some node -> return (g, env, node))
     | None ->
       (match add_node g env None label props with
        | Ok (g, env, node) -> return (g, env, node)
        | Error error -> fail error))
;;

let save_edge g env var vlabels vprops n1 n2 =
  match n1, n2 with
  | (elm1, (_, _)), (elm2, (_, _)) ->
    let edge = (vlabels, vprops), (Some elm1, Some elm2) in
    let crt_edge = LGraph.Edge.create n1 n2 edge in
    let graph = LGraph.Edge.insert crt_edge g in
    let env =
      match var with
      | None -> env
      | Some var -> extend_by_one var edge env
    in
    return (graph, env, edge)
;;

let add_edge g env n1 n2 var label = function
  | Some props ->
    let* vprops = get_props env props in
    save_edge g env var label vprops n1 n2
  | None -> save_edge g env var label [] n1 n2
;;

let run_create g env elms =
  let* num_elms = return (LGraph.number_of_nodes g, LGraph.number_of_edges g) in
  let* g, env =
    List.fold_left
      ~f:(fun acc elm ->
        let* g, env = acc in
        match elm with
        | Node nodedata ->
          (match check_node g env nodedata with
           | Ok (g, env, _) -> return (g, env)
           | Error error -> fail error)
        | Edge (n1, e, n2) ->
          (match e with
           | EdgeData (DirectL (var, label, props)) ->
             (match label with
              | Some label ->
                let* g, env, n1 = check_node g env n1 in
                let* g, env, n2 = check_node g env n2 in
                (match add_edge g env n2 n1 var [ label ] props with
                 | Ok (g, env, _) -> return (g, env)
                 | Error error -> fail error)
              | None ->
                fail @@ IncorrectCreate "To create an edge, you must specify a label.")
           | EdgeData (DirectR (var, label, props)) ->
             (match label with
              | Some label ->
                let* g, env, n1 = check_node g env n1 in
                let* g, env, n2 = check_node g env n2 in
                (match add_edge g env n1 n2 var [ label ] props with
                 | Ok (g, env, _) -> return (g, env)
                 | Error error -> fail error)
              | None ->
                fail @@ IncorrectCreate "To create an edge, you must specify a label.")
           | _ ->
             fail @@ IncorrectCreate "To create an edge, you must specify a direction."))
      ~init:(return (g, env))
      elms
  in
  let open Stdlib.Format in
  let n, e = num_elms in
  let nn, ne = LGraph.number_of_nodes g - n, LGraph.number_of_edges g - e in
  if nn <> 0 then printf "\nNodes created: %d \n" nn;
  if ne <> 0 then printf "Edges created: %d \n" ne;
  return (g, env)
;;

let check_match_elm mprops props =
  if List.length mprops <= List.length props
  then
    return @@ List.for_all mprops ~f:(fun mprop -> List.mem props mprop ~equal:Poly.( = ))
  else return false
;;

let check_where_elm mprops props expr var =
  if List.length mprops <= List.length props
  then
    if List.for_all mprops ~f:(fun mprop -> List.mem props mprop ~equal:Poly.( = ))
    then interpret_expr_where expr var props
    else return false
  else return false
;;

let find_elms_match env mprops mlabels = function
  | (labels, props), (_, _) ->
    (match mlabels, mprops with
     | Some mlabels, Some mprops ->
       let* mprops = get_props env mprops in
       if List.length mlabels <= List.length labels
       then
         if List.for_all mlabels ~f:(fun mlabel ->
              List.mem labels mlabel ~equal:Poly.( = ))
         then check_match_elm mprops props
         else return false
       else return false
     | Some mlabels, None -> check_match_elm mlabels labels
     | None, Some mprops ->
       let* mprops = get_props env mprops in
       check_match_elm mprops props
     | None, None -> return true)
;;

let find_elms_m_where env var mlabels mprops expr = function
  | (labels, props), (_, _) ->
    (match mlabels, mprops with
     | Some mlabels, Some mprops ->
       let* mprops = get_props env mprops in
       if List.length mlabels <= List.length labels
       then
         if List.for_all mlabels ~f:(fun mlabel ->
              List.mem labels mlabel ~equal:Poly.( = ))
         then check_where_elm mprops props expr var
         else return false
       else return false
     | Some mlabels, None ->
       if List.length mlabels <= List.length labels
       then
         if List.for_all mlabels ~f:(fun mlabel ->
              List.mem labels mlabel ~equal:Poly.( = ))
         then interpret_expr_where expr var props
         else return false
       else return false
     | None, Some mprops ->
       let* mprops = get_props env mprops in
       check_where_elm mprops props expr var
     | None, None -> interpret_expr_where expr var props)
;;

let find_elms env var mlabels mprops elm felms typeofelm = function
  | None ->
    let boolean = find_elms_match env mprops mlabels elm in
    (match boolean with
     | Ok true -> return (extend_by_one var elm env, extend_by_one var elm felms)
     | Ok false -> return (env, felms)
     | Error error -> fail error)
  | Some (CMWhere expr) ->
    let boolean = find_elms_m_where env var mlabels mprops expr elm in
    (match boolean with
     | Ok true -> return (extend_by_one var elm env, extend_by_one var elm felms)
     | Ok false -> return (env, felms)
     | Error UnstableElm ->
       (match typeofelm with
        | Edge_elm -> return (extend_by_one var elm env, extend_by_one var elm felms)
        | Node_elm -> return (env, felms))
     | Error error -> fail error)
;;

let iter_edges var mlabels mprops cwm env fn1 fn2 fedges edges =
  List.fold_left
    ~f:(fun acc edge ->
      let* env, fedges = acc in
      let fedges_len =
        List.length @@ Map.Poly.find_multi fedges (Option.value var ~default:"")
      in
      let* _, fedges =
        find_elms
          env
          (Option.value var ~default:"")
          (match mlabels with
           | None -> None
           | Some mlabels -> Some [ mlabels ])
          mprops
          edge
          fedges
          Edge_elm
          cwm
      in
      if List.length @@ Map.Poly.find_multi fedges (Option.value var ~default:"")
         > fedges_len
      then (
        let list = [ fn1; Option.value var ~default:"", edge; fn2 ] in
        let* env =
          return
          @@ List.fold ~f:(fun env (var, elm) -> extend_by_one var elm env) ~init:env list
        in
        return (env, fedges))
      else return (env, fedges))
    ~init:(return (env, fedges))
    edges
;;

let check_type_edges cwm g env fn1 fn2 fedges elm1 elm2 e =
  let all_edges = LGraph.edges g in
  let find_edge_list node1 node2 =
    Sequence.fold
      ~f:(fun acc edge ->
        let* list = acc in
        match edge with
        | n1, edata, n2 ->
          if Poly.( = ) n1 node1 && Poly.( = ) n2 node2
          then return @@ (edata :: list)
          else return list)
      ~init:(return [])
      all_edges
  in
  match e with
  | EdgeData (DirectL (var, mlabels, mprops)) ->
    let* edge_list = find_edge_list elm2 elm1 in
    iter_edges var mlabels mprops cwm env fn1 fn2 fedges edge_list
  | EdgeData (DirectR (var, mlabels, mprops)) ->
    let* edge_list = find_edge_list elm1 elm2 in
    iter_edges var mlabels mprops cwm env fn1 fn2 fedges edge_list
  | EdgeData (UnDirect (var, mlabels, mprops)) ->
    let* edge_list1 = find_edge_list elm2 elm1 in
    let* edge_list2 = find_edge_list elm1 elm2 in
    iter_edges var mlabels mprops cwm env fn1 fn2 fedges (edge_list1 @ edge_list2)
;;

let find_nodes cwm g env var mlabels mprops typeofelm =
  let all_nodes = LGraph.nodes g in
  Sequence.fold
    ~f:(fun acc node ->
      let* env, nodes = acc in
      find_elms env var mlabels mprops node nodes typeofelm cwm)
    ~init:(return (env, Map.Poly.empty))
    all_nodes
;;

let find_edges g env cwm n1 e n2 =
  let* _, fn1 =
    match n1 with
    | NodeData (var, labels, props) ->
      find_nodes cwm g Map.Poly.empty (Option.value var ~default:"") labels props Edge_elm
  in
  let* _, fn2 =
    match n2 with
    | NodeData (var, labels, props) ->
      find_nodes cwm g Map.Poly.empty (Option.value var ~default:"") labels props Edge_elm
  in
  let map_to_list map =
    Map.Poly.fold
      ~f:(fun ~key:var ~data:elm_l acc ->
        let* elmlist = acc in
        List.fold_left
          ~f:(fun acc elm ->
            let* elmlist = acc in
            return ((var, elm) :: elmlist))
          ~init:(return elmlist)
          elm_l)
      ~init:(return [])
      map
  in
  let* list_n1 = map_to_list fn1 in
  let* list_n2 = map_to_list fn2 in
  List.fold_left
    ~f:(fun acc fnode1 ->
      let* env, fedges = acc in
      List.fold_left
        ~f:(fun acc fnode2 ->
          let* env, fedges = acc in
          match fnode1, fnode2 with
          | (_, elm1), (_, elm2) ->
            check_type_edges cwm g env fnode1 fnode2 fedges elm1 elm2 e)
        ~init:(return (env, fedges))
        list_n2)
    ~init:(return (env, Map.Poly.empty))
    list_n1
;;

let run_detach_delete g env vars =
  let* env_len = length env in
  if List.length vars <= env_len
  then
    if List.for_all vars ~f:(fun var -> Map.Poly.mem env var)
    then (
      let* num_elms = return (LGraph.number_of_nodes g, LGraph.number_of_edges g) in
      let* g, env =
        List.fold_left
          ~f:(fun acc var ->
            let* g, env = acc in
            let elmlist = Map.Poly.find_multi env var in
            List.fold_left
              ~f:(fun acc elm ->
                let* g, env = acc in
                match elm with
                | (_, _), (Some src, Some dst) ->
                  return
                    ( LGraph.Edge.remove ((src, (None, None)), (dst, (None, None)), elm) g
                    , env )
                | _ ->
                  List.fold_left
                    ~f:(fun acc node ->
                      let* g, env = acc in
                      match LGraph.Node.edge node elm g with
                      | Some edge ->
                        let* g = return @@ LGraph.Edge.remove edge g in
                        return (LGraph.Node.remove elm g, env)
                      | None ->
                        (match LGraph.Node.edge elm node g with
                         | Some edge ->
                           let* g = return @@ LGraph.Edge.remove edge g in
                           return (LGraph.Node.remove elm g, env)
                         | None -> return (g, env)))
                    ~init:(return (g, env))
                    (Sequence.to_list @@ LGraph.nodes g))
              ~init:(return (g, env))
              elmlist)
          ~init:(return (g, env))
          vars
      in
      let open Stdlib.Format in
      let n, e = num_elms in
      let nn, ne = n - LGraph.number_of_nodes g, e - LGraph.number_of_edges g in
      if nn <> 0 then printf "\nNodes deleted: %d \n" nn;
      if ne <> 0 then printf "Edges deleted: %d \n" ne;
      return (g, env))
    else fail @@ UnboundValue "Undefined variable or nothing was found."
  else fail @@ UnboundValue "Undefined variable or nothing was found."
;;

let run_delete g env vars =
  let* env_len = length env in
  if List.length vars <= env_len
  then
    if List.for_all vars ~f:(fun var -> Map.Poly.mem env var)
    then (
      let* num_elms = return (LGraph.number_of_nodes g, LGraph.number_of_edges g) in
      let* g, env =
        List.fold_left
          ~f:(fun acc var ->
            let* g, env = acc in
            let elmlist = Map.Poly.find_multi env var in
            List.fold_left
              ~f:(fun acc elm ->
                let* g, env = acc in
                match elm with
                | (_, _), (Some src, Some dst) ->
                  return
                    ( LGraph.Edge.remove ((src, (None, None)), (dst, (None, None)), elm) g
                    , env )
                | (_, _), (None, None) ->
                  if List.for_all
                       (Sequence.to_list @@ LGraph.nodes g)
                       ~f:(fun node ->
                         not
                           (LGraph.Node.has_edge elm node g
                           || LGraph.Node.has_edge node elm g))
                  then return (LGraph.Node.remove elm g, env)
                  else return (g, env)
                | _ -> return (g, env))
              ~init:(return (g, env))
              elmlist)
          ~init:(return (g, env))
          vars
      in
      let open Stdlib.Format in
      let n, e = num_elms in
      let nn, ne = n - LGraph.number_of_nodes g, e - LGraph.number_of_edges g in
      if nn <> 0 then printf "\nNodes deleted: %d \n" nn;
      if ne <> 0 then printf "Edges deleted: %d \n" ne;
      return (g, env))
    else fail @@ UnboundValue "Undefined variable or nothing was found."
  else fail @@ UnboundValue "Undefined variable or nothing was found."
;;

let run_return g env exprs =
  let* returnlist =
    List.fold_left
      ~f:(fun acc expr ->
        let* returnlist = acc in
        let* valuelist = interpret_expr env expr in
        return ((expr, List.rev valuelist) :: returnlist))
      ~init:(return [])
      exprs
  in
  let open Stdlib.Format in
  List.fold_left
    ~f:(fun acc exprvalreturn ->
      let expr, valuereturn = exprvalreturn in
      let* expr = interpret_expr_for_ret expr in
      printf
        "\n+-----------------------------+\n%s\n+-----------------------------+\n%!"
        expr;
      let* graph, env = acc in
      List.fold_left
        ~f:(fun acc elmret ->
          let* g, env = acc in
          match elmret with
          | Value value ->
            pp_value std_formatter value;
            printf "\n";
            return (g, env)
          | VElm elm ->
            (match elm with
             | (labels, props), (Some _, Some _) ->
               printf "\n\"type\": %a\n\"properties\": %a\n" pp_type labels pp_props props;
               return (g, env)
             | (labels, props), (_, _) ->
               printf
                 "\n\"labels\": %a\n\"properties\": %a\n"
                 pp_labels
                 labels
                 pp_props
                 props;
               return (g, env)))
        ~init:(return (graph, env))
        valuereturn)
    ~init:(return (g, env))
    (List.rev returnlist)
;;

let run_cmd env = function
  | CmdCreate elms ->
    let g, env = env in
    (match run_create g env elms with
     | Ok (g, env) -> return (g, env)
     | Error error -> fail error)
  | CmdMatch (elms, cmd_with_match, cmd_match) ->
    let* env =
      List.fold_left
        ~f:(fun acc elm ->
          let* g, env = acc in
          match elm with
          | Node (NodeData (var, labels, props)) ->
            (match var with
             | Some var ->
               let* env, _ = find_nodes cmd_with_match g env var labels props Node_elm in
               return (g, env)
             | None -> return (g, env))
          | Edge (n1, e, n2) ->
            let* env, _ = find_edges g env cmd_with_match n1 e n2 in
            return (g, env))
        ~init:(return env)
        elms
    in
    List.fold_left
      ~f:(fun acc cmd ->
        let* g, env = acc in
        match cmd with
        | CMCreate elms ->
          (match run_create g env elms with
           | Ok (g, env) -> return (g, env)
           | Error error -> fail error)
        | CMReturn exprs -> run_return g env exprs
        | CMDetachDelete vars -> run_detach_delete g env vars
        | CMDelete vars -> run_delete g env vars)
      ~init:(return env)
      cmd_match
;;

let run cmds =
  List.fold_left
    ~f:(fun acc cmd ->
      let* g, _ = acc in
      match run_cmd (g, Map.Poly.empty) cmd with
      | Ok (g, _) -> return (g, Map.Poly.empty)
      | Error error -> fail error)
    ~init:(return empty)
    cmds
;;
