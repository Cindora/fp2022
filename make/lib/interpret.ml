(** Copyright 2022-2023, ioannessh and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Ast
open Graphlib.Std
open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

exception NoRule of string
exception NoRuleNeed of string * string
exception IsUpToDate of string
exception RuleError of string * int
exception NothingToBeDone of string
exception AllDone

module Node_impl = struct
  module T = struct
    type t = string [@@deriving compare, hash, sexp]
  end

  include T
  include Core.Comparable.Make (T)
  include Core.Hashable.Make (T)
end

module Node : sig
  type t = string [@@deriving compare, hash, sexp]

  include Core.Comparable.S with type t := t
  include Core.Hashable.S with type t := t
end =
  Node_impl

(*= Init Graph =*)
module G = Graphlib.Labeled (Node) (Unit) (Unit)
module VMap = Map.Make (Node)

type dfs_state =
  { in_tree : bool
  ; marked_nodes : G.Node.Set.t
  }

(* Substitute variable values in string *)
let insert_vars_values vars_map (targets_as_string : substring list) =
  let var_value name =
    match VMap.find_opt name vars_map with
    | None -> ""
    | Some s -> s
  in
  let get_str = function
    | STR s -> s
    | VAR v -> var_value v
  in
  List.fold_left (fun init x -> init ^ get_str x) "" targets_as_string
;;

let insert_vars vars_map targets =
  let targets_as_words = Parser.substrings_parser (targets ^ " ") in
  insert_vars_values vars_map targets_as_words
;;

(*== Fill maps ==*)
(*= Fill vars map =*)
let fill_var_map vars_map exprs =
  let set_var name value map =
    if VMap.mem name map
    then VMap.update name (fun _ -> Some value) map
    else VMap.add name value map
  in
  let choise_decl map = function
    | RULE _ -> map
    | VAR_DEC (name, value) -> set_var name value map
  in
  List.fold_left choise_decl vars_map exprs
;;

(*= Fill rules and prerequisites map =*)
let set_rules targets rules map =
  let set_rule map target =
    if VMap.mem target map
    then (
      Printf.eprintf
        "Makefile: warning: overriding recipe for target '%s'\n\
         Makefile: warning: ignoring old recipe for target '%s'\n"
        target
        target;
      VMap.update target (fun _ -> Some rules) map)
    else VMap.add target rules map
  in
  List.fold_left set_rule map targets
;;

let set_prereqs targets prereqs map =
  let set_prereq map target =
    if VMap.mem target map
    then VMap.update target (fun l -> Some (List.append prereqs (Option.get l))) map
    else VMap.add target prereqs map
  in
  List.fold_left set_prereq map targets
;;

let fill_maps vars_map exprs =
  let targets rule = words_parser (insert_vars vars_map rule.targets) in
  let prerequisites rule = words_parser (insert_vars vars_map rule.prerequisites) in
  let choise_decl (rules_map, prereqs_map) = function
    | RULE rule ->
      ( set_rules (targets rule) rule.commands rules_map
      , set_prereqs (targets rule) (prerequisites rule) prereqs_map )
    | VAR_DEC _ -> rules_map, prereqs_map
  in
  List.fold_left choise_decl (VMap.empty, VMap.empty) exprs
;;

(* Fill Graph *)
let create_vertex (map : G.node VMap.t) target =
  match VMap.find_opt target map with
  | None ->
    let vertex = G.Node.create { node = target; node_label = () } in
    let map = VMap.add target vertex map in
    vertex, map
  | Some vertex -> vertex, map
;;

let create_graph target_list prereqs_map =
  let map : G.node VMap.t = VMap.empty in
  let graph : G.t = G.empty in
  let insert_vertex (map, graph) target =
    let parent, map = create_vertex map target in
    let graph = G.Node.insert parent graph in
    let prereqs = VMap.find target prereqs_map in
    let insert_edge (map, graph) target =
      let child, map = create_vertex map target in
      let graph = G.Edge.insert (G.Edge.create parent child ()) graph in
      map, graph
    in
    let map, graph = List.fold_left insert_edge (map, graph) prereqs in
    map, graph
  in
  List.fold_left insert_vertex (map, graph) target_list
;;

(* Traverse Graph(dfs) *)
let dfs node graph enter_node leave_node =
  let reachable =
    Graphlib.fold_reachable (module G) ~init:G.Node.Set.empty ~f:G.Node.Set.add graph node
  in
  Graphlib.depth_first_search
    (module G)
    graph
    ~start:node
    ~start_tree:(fun node state ->
      if G.Node.Set.mem reachable node then state else { state with in_tree = true })
    ~init:{ in_tree = false; marked_nodes = G.Node.Set.empty }
    ~leave_edge:(fun kind edge state ->
      match kind with
      | `Back ->
        Printf.eprintf
          "make: Circular %s <- %s dependency dropped.\n"
          (G.Edge.src edge).node
          (G.Edge.dst edge).node;
        state
      | _ -> state)
    ~enter_node
    ~leave_node
;;

(* Run rules *)
let exec_rule rule vars_map target =
  let choise_decl str =
    let command = insert_vars_values vars_map str in
    let retcode =
      if String.starts_with ~prefix:"@" command
      then (
        let command = String.mapi (fun i c1 -> if 0 = i then ' ' else c1) command in
        Sys.command command)
      else (
        print_endline command;
        Sys.command command)
    in
    if retcode <> 0 then raise (RuleError (target, retcode)) else ()
  in
  List.iter choise_decl rule
;;

let compare_time_stats_of_childs graph node =
  let target = node.node in
  let compare_time_stats target child =
    (not (Sys.file_exists child))
    || (Unix.stat child).st_mtime > (Unix.stat target).st_mtime
  in
  Base.Sequence.exists (G.Node.succs node graph) ~f:(fun child ->
    compare_time_stats target child.node)
;;

let try_exec rules_map vars_map prereqs_map graph (node : G.Node.t) marked_nodes goal =
  let target = node.node in
  let vars_map = VMap.update "@" (fun _ -> Some target) vars_map in
  let self_prerequisites =
    match VMap.find_opt target prereqs_map with
    | None -> ""
    | Some s -> String.concat " " s
  in
  let vars_map = VMap.update "^" (fun _ -> Some self_prerequisites) vars_map in
  if Base.Sequence.exists (G.Node.succs node graph) ~f:(fun child ->
       G.Node.Set.mem marked_nodes child)
  then (
    exec_rule (VMap.find target rules_map) vars_map target;
    G.Node.Set.add marked_nodes node)
  else if Sys.file_exists target
  then
    if compare_time_stats_of_childs graph node
    then (
      exec_rule (VMap.find target rules_map) vars_map target;
      G.Node.Set.add marked_nodes node)
    else if target = goal
    then raise (IsUpToDate target)
    else marked_nodes
  else (
    match VMap.find_opt target rules_map with
    | None -> raise (NoRuleNeed (target, goal))
    | Some [] -> raise (NothingToBeDone target)
    | Some rule ->
      exec_rule rule vars_map target;
      G.Node.Set.add marked_nodes node)
;;

(* Run DFS on graph *)
let run_dfs rules_map vars_map prereqs_map graph vertex_map target =
  if (not (VMap.mem target vertex_map)) || not (VMap.mem target rules_map)
  then raise (NoRule target)
  else (
    let start_node = VMap.find target vertex_map in
    let enter_n _ (_ : G.Node.t) stat = stat in
    let leave_n _ (node : G.Node.t) stat =
      if stat.in_tree
      then stat
      else
        { stat with
          marked_nodes =
            try_exec rules_map vars_map prereqs_map graph node stat.marked_nodes target
        }
    in
    dfs start_node graph enter_n leave_n |> fun _ -> ())
;;

(*===== INTERPRETER =====*)
let exec_exprs exprs main_targets =
  let vars_map = fill_var_map VMap.empty exprs in
  let vars_map = VMap.add "@" "@" vars_map in
  let vars_map = VMap.add "^" "^" vars_map in
  let vars_map = VMap.add "$" "$" vars_map in
  let rules_map, prereqs_map = fill_maps vars_map exprs in
  let targets =
    VMap.fold (fun target _ targets -> List.append [ target ] targets) rules_map []
  in
  let vartex_map, graph = create_graph targets prereqs_map in
  try
    List.iter (run_dfs rules_map vars_map prereqs_map graph vartex_map) main_targets;
    raise AllDone
  with
  | NoRule target ->
    Printf.eprintf "make: *** No rule to make target `%s`. Stop.\n" target
  | IsUpToDate target -> Printf.printf "make: `%s` is up to date.\n" target
  | RuleError (err, retcode) ->
    Printf.eprintf "make: *** [Makefile: %s] Error %d\n" err retcode
  | NothingToBeDone target -> Printf.printf "make: Nothing to be done for `%s`.\n" target
  | NoRuleNeed (prereq, goal) ->
    Printf.eprintf "No rule to make target `%s`, needed by `%s`.\n" prereq goal
  | AllDone -> ()
  | InvalidVarName s -> Printf.eprintf "Char '%s' cannot be used in variable name. \n" s
  | InvalidString (input, _) -> Printf.eprintf "String \"%s\" cannot be parsed\n" input
;;

let interpret input targets =
  let targets = if 0 = List.length targets then [ "all" ] else targets in
  match parser input with
  | Result.Ok res -> exec_exprs res targets
  | Error e -> print_string e
;;

let args_list xs =
  let list = Array.fold_right List.cons xs [] in
  match list with
  | _ :: args -> args
  | _ -> []
;;

(*=============================*)
(*============TESTS============*)
(*=============================*)
let test_vars = VMap.add "t1" "some" VMap.empty
