(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type key_setup = string [@@deriving show]

type field_setup =
  { field_type : types
  ; field_key : key_setup
  ; is_const : bool
  ; sub_tree : expr option
  }
[@@deriving show { with_path = false }]

type method_setup =
  { method_type : types
  ; method_key : key_setup
  ; args : (types * string) list
  ; body : statement
  }
[@@deriving show { with_path = false }]

module KeyMap = struct
  include Map.Make (String)

  let pp pp_v ppf map =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%S\": %a@],@\n" k pp_v v) map;
    Format.fprintf ppf "@]]@]"
  ;;
end

type class_setup =
  { class_key : key_setup
  ; field_map : field_setup KeyMap.t
  ; method_map : method_setup KeyMap.t
  ; parent_key : key_setup option
  ; declare_class : classes
  }
[@@deriving show { with_path = false }]

and error =
  | DivisionByZero
  | IncorrectType
  | NoMain
  | NoFindInMain of string
  | NoFindClass of string
  | NoFindMethod of string
  | NoVariable of string
  | ThisNameAlreadyExists of string
  | TypeMismatch
  | NotBrackets
  | IncorrectArgumentType
  | BrokeDown
  | IncorrectSyntax

let str_of_error = function
  | DivisionByZero -> "Division by zero"
  | IncorrectType -> "Incorrect type"
  | IncorrectArgumentType -> "Incorrect argument type"
  | NoMain -> "There is no Main class!"
  | NoFindInMain arg -> "In the Main class is no find method " ^ arg
  | NoFindClass arg -> "This class wasn't found: " ^ arg
  | NoFindMethod arg -> "This method wasn't found: " ^ arg
  | NoVariable arg -> "There is no the variable " ^ arg
  | ThisNameAlreadyExists arg -> "This name already exists" ^ arg
  | TypeMismatch -> "Return value type mismatch"
  | NotBrackets -> "Expected { }"
  | BrokeDown -> "Broke down"
  | IncorrectSyntax -> "Incorrect syntax, ooops"
;;
