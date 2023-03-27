(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int
type identifier = string

type ground_type =
  | String
  | Int
  | Bool

type typ =
  | TVar of type_variable_number * bool (** 'a, '~A : bool for restricted value *)
  | TEqualityVar of type_variable_number * bool
      (** ''a, ''~A : bool for restricted equality value *)
  | TArr of typ * typ (** string -> int *)
  | TTuple of typ list (** int * int *)
  | TList of typ (** 'a list *)
  | TOption of typ (** (int, string) Result *)
  | TGround of ground_type (** int *)

(* Ground types *)
let string_typ = TGround String
let int_typ = TGround Int
let bool_typ = TGround Bool
let var_t n = TVar (n, false)
let eqvar_t n = TEqualityVar (n, false)
let val_t n = TVar (n, true)
let eqval_t n = TEqualityVar (n, true)
let arrow_t left_type right_type = TArr (left_type, right_type)
let tuple_t type_list = TTuple type_list
let option_t typ = TOption typ
let list_t typ = TList typ

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of identifier (** Using undefined variable *)
  | `UnificationFailed of typ * typ (** Unify of typs failed *)
  | `Unreachable (** Bug in parser *)
  | `Not_function
  ]

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | String -> fprintf fmt "string"
     | Bool -> fprintf fmt "bool")
  | TTuple value_list ->
    fprintf
      fmt
      "(%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ -> pp_type fmt typ))
      value_list;
    fprintf fmt ")"
  | TList typ -> fprintf fmt (arrow_format typ ^^ " list") pp_type typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TOption typ -> fprintf fmt "%a option" pp_type typ
  | TVar (var, false) ->
    fprintf fmt "%s" @@ "'" ^ Char.escaped (Stdlib.Char.chr (var + 97))
  | TEqualityVar (var, false) ->
    fprintf fmt "%s" @@ "''" ^ Char.escaped (Stdlib.Char.chr (var + 97))
  | TVar (var, true) ->
    fprintf fmt "%s" @@ "'~" ^ Char.escaped (Stdlib.Char.chr (var + 65))
  | TEqualityVar (var, true) ->
    fprintf fmt "%s" @@ "''~" ^ Char.escaped (Stdlib.Char.chr (var + 65))
;;

let print_typ typ = Format.printf "%a\n" pp_type typ

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmt "Occurs check failed."
  | `NoVariable identifier ->
    fprintf fmt "Elaboration failed: Unbound value identifier %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf
      fmt
      "Elaboration failed: Rules disagree on type: Cannot merge %a and %a"
      pp_type
      t1
      pp_type
      t2
  | `Unreachable -> fprintf fmt "Not reachable."
  | `Not_function -> fprintf fmt "Applying not a function"
;;

let print_type_error error = Format.printf "%a\n" pp_error error
