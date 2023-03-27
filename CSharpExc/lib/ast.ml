(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type types =
  | Float
  | Int
  | Bool
  | Class of string
  | Void
  | String
[@@deriving show { with_path = false }]

type modifier =
  | Public
  | Static
  | Override
  | Const
[@@deriving show { with_path = false }]

type values =
  | Int' of int
  | Float' of float
  | Bool' of bool
  | Void'
  | Null'
  | String' of string
  | Class' of object_refers
[@@deriving show { with_path = false }]

and object_refers =
  | ObjectNull
  | ObjectRef of string * string option (** key * parent key *)
[@@deriving show { with_path = false }]

type bin_ops =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Mod
[@@deriving show { with_path = false }]

type compares =
  | Equal
  | NotEqual
  | Less
  | More
  | LessOrEqual
  | MoreOrEqual
[@@deriving show { with_path = false }]

type expr =
  | Value of values
  | Variable of string
  | Binop of bin_ops * expr * expr
  | Compare of compares * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Null
  | IncrementPost of expr
  | DecrementPost of expr
  | ClassCreate of string * expr list
  | CallMethod of string * expr list
  | Assign of expr * expr
[@@deriving show { with_path = false }]

and statement =
  | Expr of expr
  | Block of statement list
  | If of expr * statement * statement option (** condition * if body * else body *)
  | For of statement option * expr option * expr list * statement
      (** declare variable * condition * (multiple variable in condition) * for body *)
  | While of expr * statement (** condition * while body *)
  | Throw of expr
  | Try of statement * (types option * statement) list * statement option
      (** try body - catch body - finally body *)
  | Break
  | Continue
  | Return of expr option
  | VariableDeclare of modifier option * types * (string * expr option) list
      (** modifier * type * declare *)
  | Print of expr
[@@deriving show { with_path = false }]

and fields =
  | VariableField of types * (string * expr option) list (** type * variables *)
  | Method of types * string * (types * string) list * statement
      (** type * name * params * method body *)
[@@deriving show { with_path = false }]

and classes =
  | ClassDec of modifier list * string * string option * (modifier list * fields) list
      (** modifier * name * parents * class body *)
[@@deriving show { with_path = false }]
