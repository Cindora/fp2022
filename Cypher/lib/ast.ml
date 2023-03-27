(** Copyright 2022-2023, Drumov and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Constant type *)
type constant =
  | CString of string (** String literals "Something"*)
  | CInt of int (** Integer literals 123 *)
[@@deriving show { with_path = false }]

(** Binary operations type *)
type binary_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | NEq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | LEq (** <= *)
  | GEq (** >= *)
  | And (** && *)
  | Or (** || *)
  | KWAnd (** Keyword AND *)
  | KWOr (** Keyword OR *)
  | Xor (** Keyword XOR *)
  | As (** Keyword AS *)
[@@deriving show { with_path = false }]

(** Unary operations type *)
type unary_op =
  | Not (** ! *)
  | KWNot (** NOT *)
  | IsNull (** NULL *)
  | IsNotNull (** NOT NULL *)
[@@deriving show { with_path = false }]

(** Expression type *)
type expr =
  | EConst of constant (** An expression for the constata *)
  | EGetProp of string * string (** An expression to get the properties *)
  | EGetType of string (** An expression to get the type of an edge *)
  | EGetElm of string (** An expression to get edges and nodes *)
  | EGetAlias of string (** An expression to get the alias for the AS operation *)
  | EBinop of binary_op * expr * expr (** An expression for binary operations *)
  | EUnop of unary_op * expr (** An expression for unary operations*)
[@@deriving show { with_path = false }]

(** {name:"Ann"} *)
type property = string * expr [@@deriving show { with_path = false }]

(**  (var, label, property)
    [edge: ACTED_IN {role: 'President Andrew Shepherd'}] *)
type direction = string option * string option * property list option
[@@deriving show { with_path = false }]

type edge_direction =
  | DirectR of direction
  | DirectL of direction
  | UnDirect of direction
[@@deriving show { with_path = false }]

(** (var, label, property)
    var - element name(Used to name different parts in (n)--(m) queries. n and m are variables),
    labels - similar to tags and allow you to specify certain types of entities to search for or create,
    properties - name-value pairs that provide additional details to our nodes and relationships.
    [edge : PARENT { role: "Father" }] *)
type edge_data = EdgeData of edge_direction [@@deriving show { with_path = false }]

(** (var, label, property)
    var - element name(Used to name different parts in (n)--(m) queries. n and m are variables),
    labels - similar to tags and allow you to specify certain types of entities to search for or create,
    properties - name-value pairs that provide additional details to our nodes and relationships.
    (node : PERSON { name: "Ann" }) *)
type node_data = NodeData of string option * string list option * property list option
[@@deriving show { with_path = false }]

type elm =
  | Node of node_data (** (node_data) *)
  | Edge of node_data * edge_data * node_data (** (node_data)-[edge_data]->(node_data) *)
[@@deriving show { with_path = false }]

type cmd_with_match =
  (* | CMSet of expr * expr option * property list option *)
  (* SET n.name = "Tyler" or SET n = {surname: "Derden"}*)
  | CMWhere of expr (** WHERE n.name = "Ann" AND n.age < 20 *)
[@@deriving show { with_path = false }]

type cmd_match =
  | CMCreate of elm list (** CREATE elms *)
  | CMDelete of string list (** DELETE vars *)
  | CMDetachDelete of string list (** DETACH DELETE vars *)
  | CMReturn of expr list (** RETURN vars *)
[@@deriving show { with_path = false }]

type command =
  | CmdCreate of elm list (** CREATE elms *)
  | CmdMatch of elm list * cmd_with_match option * cmd_match list
      (** MATCH elms cmd_with_match cmd_match *)
[@@deriving show { with_path = false }]

type program = command list [@@deriving show { with_path = false }]
