(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type bin_op =
  | And (**  and *)
  | Or (**  or  *)
  | Less (**  <   *)
  | Leq (**  <=  *)
  | Gre (**  >   *)
  | Geq (**  >=  *)
  | Eq (** =   *)
  | Neq (**  <>  *)
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  *   *)
  | Div (**  div   *)

and un_op =
  | Not (**  not  *)
  | Minus (** ~ *)

and binding = bool * pt * exp
and case = pt * exp
and decl = DLet of binding (**  val y = 256   *)
and prog = decl list

and const =
  | CString of string (**  "xyz"  *)
  | CInt of int (**   256    *)
  | CBool of bool (**  false   *)

and exp =
  | EConst of const (**    false    *)
  | ENone (**    NONE    *)
  | ENil (** [] *)
  | EUnOp of un_op * exp (**    not x, ~x, !x    *)
  | EVar of id (**    x    *)
  | ECons of exp * exp (**    x :: xs    *)
  | ETuple of exp list (**    x, y, z    *)
  | ELet of binding list * exp (**    let x = 256 in 512    *)
  | EFun of pt * exp (**   fn x => x   *)
  | EMatch of exp * case list (**    case x of 1 => 2 | _ => x * 1337    *)
  | ESome of exp (**    SOME a    *)
  | EIf of exp * exp * exp (**    if predicate then x else y    *)
  | EBinOp of bin_op * exp * exp (**    25 div (7 + ~2)    *)
  | EApp of exp * exp (**    fold a list init    *)

and pt =
  | PtWild (**  _  *)
  | PtVar of id (**  xyz   *)
  | PtConst of const (**  256   *)
  | PtCons of pt * pt (**  hd :: tl  *)
  | PtSome of pt (**   SOME a  *)
  | PtNone (**  NONE  *)
  | PtNil (**  []  *)
  | PtTuple of pt list (**  x, y, z   *)
[@@deriving show { with_path = false }]
