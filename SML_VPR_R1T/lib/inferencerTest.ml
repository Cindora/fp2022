(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Inferencer

let%expect_test _ =
  parse_and_inference "val x = 5";
  [%expect {| int |}]
;;

let%expect_test _ =
  parse_and_inference "val s = \"str\"";
  [%expect {| string |}]
;;

let%expect_test _ =
  parse_and_inference "val b = true";
  [%expect {| bool |}]
;;

let%expect_test _ =
  parse_and_inference "val c = \'c\'";
  [%expect {| char |}]
;;

let%expect_test _ =
  parse_and_inference "val u = ()";
  [%expect {| unit |}]
;;

let%expect_test _ =
  parse_and_inference "val m = [1,2,3]";
  [%expect {| int list |}]
;;

let%expect_test _ =
  parse_and_inference "val t = (1 , \'c\', \"str\", true)";
  [%expect {| int * char * string * bool |}]
;;

let%expect_test _ =
  parse_and_inference "fn x => x";
  [%expect {| 'a -> 'a |}]
;;

let%expect_test _ =
  parse_and_inference "let val x1 = 2 val x2 = 2 in (x1 + ~x2 / x1) * x2 end";
  [%expect {| int |}]
;;

let%expect_test _ =
  parse_and_inference
    "let val func = fn b => case b of true => true | _ => false in func false end";
  [%expect {| bool |}]
;;

let%expect_test _ =
  parse_and_inference
    "fn x => let val rec fibonacci = fn n => if n <= 0 then 0 else if n = 1 then 1 else \
     fibonacci (n-1) + fibonacci (n-2)\n\
    \    in fibonacci x end";
  [%expect {| int -> int  |}]
;;

let%expect_test _ =
  parse_and_inference
    "let val func = fn x => x + 1 val y = (\"str\", 'c', false) in ((func 1), y) end";
  [%expect {| int * string * char * bool |}]
;;

let%expect_test _ =
  parse_and_inference "let val f = fn x => if x < 0 then false else true in f (~1) end";
  [%expect {| bool |}]
;;

let%expect_test _ =
  parse_and_inference
    "let val rec concat = fn x1s => fn x2s => case x1s of [] => x2s | [x] => x::x2s | \
     hd::tl => hd::(concat tl x2s) in concat ['x', '1'] ['x', '2'] end";
  [%expect {| char list  |}]
;;

let%expect_test _ =
  parse_and_inference
    "let val max = fn x => fn y => (if x < y then y else x) in max 10 (~10) end";
  [%expect {| int |}]
;;

let%expect_test _ =
  parse_and_inference "fn x => (x, 'c', \"str\", true)";
  [%expect {|
    'a -> 'a * char * string * bool
  |}]
;;

let%expect_test _ =
  parse_and_inference "val r = fn x => if x then false else true";
  [%expect {|
    bool -> bool
  |}]
;;

let%expect_test _ =
  parse_and_inference
    "fn x1 => fn x2 => fn x3 => fn x4 => if x1 = x2 andalso x3 = x4 then true else false";
  [%expect {|
    'e -> 'e -> 'f -> 'f -> bool
  |}]
;;

let%expect_test _ =
  parse_and_inference "fn f1 => fn xs => case xs of h::t => h | [x] => x";
  [%expect {| 'a -> 'c list -> 'c |}]
;;

let%expect_test _ =
  parse_and_inference
    "fn x1 => fn x2 => fn func => fn y1 => fn y2 => x1 = x2 orelse (case y1 of [] => \
     func y2 | hd :: tl => func hd) = 0";
  [%expect {| 'f -> 'f -> ('h -> int) -> 'h list -> 'h -> bool |}]
;;

let%expect_test _ =
  parse_and_inference "fn x1 => fn x2 => x1 = true - x2 ";
  [%expect {| Unification failed: type of the expression is: bool expected type int. |}]
;;
