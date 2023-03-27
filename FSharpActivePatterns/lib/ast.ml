(** Copyright 2022-2023, Grigory Aseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type bin_op =
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  *   *)
  | Div (**  /   *)
  | Mod (**  %   *)
  | Less (**  <   *)
  | Leq (**  <=  *)
  | Gre (**  >   *)
  | Geq (**  >=  *)
  | Eq (**  =  *)
  | Neq (**  <>  *)
  | And (**  &&  *)
  | Or (**  ||  *)
  | Cons (**  ::  *)
[@@deriving eq, show { with_path = false }]

type const =
  | CBool of bool (**  true  *)
  | CInt of int (**  777  *)
  | CString of string (**  "Hello world!"  *)
  | CNil (**  []  *)
  | CUnit (**  ()  *)
[@@deriving eq, show { with_path = false }]

(** | _ :: [] -> "one elem"  *)
type case = pattern * expr

(**  | Name [arg1] [arg2] ... [argn] [val_matched]  *)
and acase = id * pattern list

and fun_title =
  | FName of id
  | FAPattern of active_pattern
[@@deriving eq, show { with_path = false }]

and active_pattern =
  | SingleChoice of bool * id (**  (|Possitive|[_|])  *)
  | MultipleChoice of id list (**  (|Even|Odd|)  *)
[@@deriving eq, show { with_path = false }]

and pattern =
  | PWild (**  _  *)
  | PConst of const (**  1  *)
  | PVar of id (**  abc  *)
  | PCons of pattern * pattern (**  hd :: tl  *)
  | PTuple of pattern list (**  a, b  *)
  | PList of pattern list (**  [1;2;3]  *)
  | PACase of acase
[@@deriving eq, show { with_path = false }]

and expr =
  | EConst of const (**  1  *)
  | EBinOp of bin_op * expr * expr (**  1 + 1  *)
  | EVar of id (**  abc  *)
  | EList of expr list (**  [1; 2; 3]  *)
  | ETuple of expr list (**  1, 2  *)
  | EIf of expr * expr * expr (**  if x > 0 then x else 0  *)
  | EFun of pattern * expr (**  fun x -> x * 2  *)
  | ELet of bool * fun_title * expr (**   let [rec] f x = e  *)
  | ELetIn of expr * expr (**  let [rec] f x = e in e'  *)
  | EApp of expr * expr (**  f x  *)
  | EMatch of expr * case list (**  match e with | _ -> 0  *)
  | EAPattern of id * expr list (**  Even  *)
[@@deriving eq, show { with_path = false }]

(**  e1 ;; e2 ;; ... ;; en;;  *)
and program = expr list [@@deriving eq, show { with_path = false }]
