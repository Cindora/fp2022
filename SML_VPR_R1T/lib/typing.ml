(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int
type identifier = string

type prime_type =
  | Char
  | String
  | Int
  | Bool
  | Unit
[@@deriving show { with_path = false }]

type type_expr =
  | TVar of type_variable_number (** 'a *)
  | TArr of type_expr * type_expr (** string -> int *)
  | TTuple of type_expr list (** int * int *)
  | TList of type_expr (** 'a list *)
  | TPrime of prime_type (** int *)

(* Ground types *)
let char_typ = TPrime Char
let string_typ = TPrime String
let int_typ = TPrime Int
let unit_typ = TPrime Unit
let bool_typ = TPrime Bool

(* Smart constructors for types *)
let var_t n = TVar n
let arrow_t left_type right_type = TArr (left_type, right_type)
let tuple_t type_list = TTuple type_list
let list_t type_expr = TList type_expr

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * type_expr

type error =
  [ `OccursCheck
  | `NoVariable of identifier
  | `UnificationFailed of type_expr * type_expr
  | `Unreachable
  ]

let rec pp_type fmtr type_expr =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match type_expr with
  | TPrime x ->
    (match x with
     | Int -> fprintf fmtr "int"
     | String -> fprintf fmtr "string"
     | Char -> fprintf fmtr "char"
     | Bool -> fprintf fmtr "bool"
     | Unit -> fprintf fmtr "unit")
  | TTuple value_list ->
    fprintf
      fmtr
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmtr " * ")
         (fun fmtr type_expr -> pp_type fmtr type_expr))
      value_list
  | TList type_expr -> fprintf fmtr (arrow_format type_expr ^^ " list") pp_type type_expr
  | TArr (typ_left, typ_right) ->
    fprintf fmtr (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmtr "%s" ("'" ^ Char.escaped (Stdlib.Char.chr (var + 97)))
;;

let print_type_expr type_expr =
  let s = Format.asprintf "%a" pp_type type_expr in
  Format.printf "%s\n" s
;;

let pp_error fmtr (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmtr "Ошибка проверки вхождения.\n"
  | `NoVariable name -> fprintf fmtr "Переменная не определена: %s" name
  | `UnificationFailed (t1, t2) ->
    fprintf fmtr "Ошибка типов, тип выражения: ";
    pp_type fmtr t1;
    fprintf fmtr " ожидался тип ";
    pp_type fmtr t2;
    fprintf fmtr "."
  | `Unreachable -> fprintf fmtr "Данный код недоступен."
;;

let print_type_expre_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;
