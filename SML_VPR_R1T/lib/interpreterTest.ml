(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpreter

let%expect_test _ =
  interpret "val x = 5";
  [%expect {| 5 |}]
;;

let%expect_test _ =
  interpret "val s = \"str\"";
  [%expect {| "str" |}]
;;

let%expect_test _ =
  interpret "val b = true";
  [%expect {| true |}]
;;

let%expect_test _ =
  interpret "val c = \'c\'";
  [%expect {| 'c' |}]
;;

let%expect_test _ =
  interpret "val u = ()";
  [%expect {| () |}]
;;

let%expect_test _ =
  interpret "val m = [1,2,3]";
  [%expect {| [1, 2, 3] |}]
;;

let%expect_test _ =
  interpret "val t = (1 , \'c\', \"str\", true)";
  [%expect {| (1, 'c', "str", true) |}]
;;

let%expect_test _ =
  interpret "fn x => x";
  [%expect {| fn |}]
;;

let%expect_test _ =
  interpret "let val x1 = 2 val x2 = 2 in (x1 + ~x2 / x1) * x2 end";
  [%expect {| 2 |}]
;;

let%expect_test _ =
  interpret "val x = 1 / 0";
  [%expect {| Деление на ноль. |}]
;;

let%expect_test _ =
  interpret "let val func = fn b => case b of true => false in func false end";
  [%expect {| Pattern Matсhing не является исчерпывающим. |}]
;;

let%expect_test _ =
  interpret "let val func = fn b => case b of true => true | _ => false in func false end";
  [%expect {| false |}]
;;

let%expect_test _ =
  interpret
    "let val fibonacci = fn n => if n <= 0 then 0 else if n = 1 then 1 else fibonacci \
     (n-1) + fibonacci (n-2)\n\
    \    in fibonacci 10 end";
  [%expect {| Ошибка присваивания значения. |}]
;;

let%expect_test _ =
  interpret
    "let val rec fibonacci = fn n => if n <= 0 then 0 else if n = 1 then 1 else \
     fibonacci (n-1) + fibonacci (n-2)\n\
    \    in fibonacci 10 end";
  [%expect {| 55 |}]
;;

let%expect_test _ =
  interpret
    "let val func = fn x => x + 1 val y = (\"str\", 'c', false) in ((func 1), y) end";
  [%expect {| ((2), ("str", 'c', false)) |}]
;;

let%expect_test _ =
  interpret "let val f = fn x => if x < 0 then false else true in f (~1) end";
  [%expect {| false |}]
;;

let%expect_test _ =
  interpret
    "let val rec sum_of_neg = fn xs => case xs of\n\
    \    [] => 0\n\
    \    | hd::tl => if hd < 0 then (hd + sum_of_neg tl) else (sum_of_neg tl)\n\
    \  in sum_of_neg [1, ~2, ~3, 4, 0] end";
  [%expect {| -5 |}]
;;

let%expect_test _ =
  interpret
    "let val x_or_null = fn x => fn y => let val get = (fn x => x) \n\
    \    val getget = (fn x => get get x)\n\
    \    in (case getget x of true => 0 | _ => (~y)) + y end \n\
    \  in x_or_null true 10 end";
  [%expect {| 10 |}]
;;

let%expect_test _ =
  interpret
    "let val rec concat = fn x1s => fn x2s => case x1s of [] => x2s | [x] => x::x2s | \
     hd::tl => hd::(concat tl x2s) in concat ['x', '1'] ['x', '2'] end";
  [%expect {| ['x', '1', 'x', '2'] |}]
;;

let%expect_test _ =
  interpret "let val max = fn x => fn y => (if x < y then y else x) in max 10 (~10) end";
  [%expect {| 10 |}]
;;

let%expect_test _ =
  interpret
    "let val rec max_in_arr = fn xs => case xs of\n\
    \    [] => 0\n\
    \    | [x] => x\n\
    \    | hd::tl => if hd > max_in_arr tl then hd else max_in_arr tl\n\
    \  in max_in_arr [1, ~2, ~3, 4, 0, 10] end";
  [%expect {| 10 |}]
;;
