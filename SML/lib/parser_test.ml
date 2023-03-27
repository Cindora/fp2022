(** Copyright 2022-2023, Rustam Shangareev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

let test_parse ~label ~code ~expected =
  match parse prog code with
  | Error _ ->
    Printf.printf "[Parser test] %s -> PARSE ERROR" label;
    false
  | Result.Ok res when expected = res -> true
  | Result.Ok res ->
    let () =
      Printf.printf "[Parser test] %s failed.\nActual is:\n%s\n" label (show_prog res)
    in
    false
;;

let%test _ =
  test_parse
    ~label:"Fun and val test"
    ~code:
      {|

    val f = fn x => fn y => fn z => (x + y * z) div 2 * 1337 + 9 div 13 - 14;;

    fun f x y z = (x + y * z) div 2 * 1337 + 9 div 13 - 14;;

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ; DLet
          ( true
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EFun
                  ( PtVar "y"
                  , EFun
                      ( PtVar "z"
                      , EBinOp
                          ( Add
                          , EBinOp
                              ( Div
                              , EBinOp (Add, EVar "x", EBinOp (Mul, EVar "y", EVar "z"))
                              , EBinOp (Mul, EConst (CInt 2), EConst (CInt 1337)) )
                          , EBinOp
                              ( Sub
                              , EBinOp (Div, EConst (CInt 9), EConst (CInt 13))
                              , EConst (CInt 14) ) ) ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Options and matching test"
    ~code:
      {|

    val matching = fn [NONE, SOME x, SOME y] => 42 | SOME a :: b => 228 | _ => 1337

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "matching"
          , EFun
              ( PtVar "case"
              , EMatch
                  ( EVar "case"
                  , [ ( PtCons
                          ( PtNone
                          , PtCons (PtSome (PtVar "x"), PtCons (PtSome (PtVar "y"), PtNil))
                          )
                      , EConst (CInt 42) )
                    ; PtCons (PtSome (PtVar "a"), PtVar "b"), EConst (CInt 228)
                    ; PtWild, EConst (CInt 1337)
                    ] ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"val rec, case test"
    ~code:
      {|

    val rec x = fn | 0 => 1 | _ => x(y - 1);;

    val t = fn c => case c of 0 => 1337 | _ => c;;

    val tmp = fn x => (if x >= 2 then SOME x else NONE, x);;

    val f = fn x => ((case x of SOME t => t | NONE => ~1))

    |}
    ~expected:
      [ DLet
          ( true
          , PtVar "x"
          , EFun
              ( PtVar "case"
              , EMatch
                  ( EVar "case"
                  , [ PtConst (CInt 0), EConst (CInt 1)
                    ; PtWild, EApp (EVar "x", EBinOp (Sub, EVar "y", EConst (CInt 1)))
                    ] ) ) )
      ; DLet
          ( false
          , PtVar "t"
          , EFun
              ( PtVar "c"
              , EMatch
                  (EVar "c", [ PtConst (CInt 0), EConst (CInt 1337); PtWild, EVar "c" ])
              ) )
      ; DLet
          ( false
          , PtVar "tmp"
          , EFun
              ( PtVar "x"
              , ETuple
                  [ EIf (EBinOp (Geq, EVar "x", EConst (CInt 2)), ESome (EVar "x"), ENone)
                  ; EVar "x"
                  ] ) )
      ; DLet
          ( false
          , PtVar "f"
          , EFun
              ( PtVar "x"
              , EMatch
                  ( EVar "x"
                  , [ PtSome (PtVar "t"), EVar "t"
                    ; PtNone, EUnOp (Minus, EConst (CInt 1))
                    ] ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"fun test"
    ~code:
      {|

    fun fact n = if n = 0 then 1 else n * fact (n-1);;

    val x = fact(3);;

    val _ = fact 5;;

    |}
    ~expected:
      [ DLet
          ( true
          , PtVar "fact"
          , EFun
              ( PtVar "n"
              , EIf
                  ( EBinOp (Eq, EVar "n", EConst (CInt 0))
                  , EConst (CInt 1)
                  , EBinOp
                      ( Mul
                      , EVar "n"
                      , EApp (EVar "fact", EBinOp (Sub, EVar "n", EConst (CInt 1))) ) ) )
          )
      ; DLet (false, PtVar "x", EApp (EVar "fact", EConst (CInt 3)))
      ; DLet (false, PtWild, EApp (EVar "fact", EConst (CInt 5)))
      ]
;;

let%test _ =
  test_parse
    ~label:"list and let .. in .. end test"
    ~code:
      {|

    val x = f [1, 2, 3];;

    val t = let val x = 3 val y = 4 in x + y end;;
  

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "x"
          , EApp
              ( EVar "f"
              , ECons
                  (EConst (CInt 1), ECons (EConst (CInt 2), ECons (EConst (CInt 3), ENil)))
              ) )
      ; DLet
          ( false
          , PtVar "t"
          , ELet
              ( [ false, PtVar "x", EConst (CInt 3); false, PtVar "y", EConst (CInt 4) ]
              , EBinOp (Add, EVar "x", EVar "y") ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"fib test"
    ~code:
      {|

      fun fib n = if n = 0 orelse n = 1 then 1 else fib(n - 1) + fib(n - 2);;

      val tmp = fib 15 14 3;;

      val tmp =
      let
        val check = fn f => fn x => if f x = x then f x else x
      in
        check
      end;;

    |}
    ~expected:
      [ DLet
          ( true
          , PtVar "fib"
          , EFun
              ( PtVar "n"
              , EIf
                  ( EBinOp
                      ( Or
                      , EBinOp (Eq, EVar "n", EConst (CInt 0))
                      , EBinOp (Eq, EVar "n", EConst (CInt 1)) )
                  , EConst (CInt 1)
                  , EBinOp
                      ( Add
                      , EApp (EVar "fib", EBinOp (Sub, EVar "n", EConst (CInt 1)))
                      , EApp (EVar "fib", EBinOp (Sub, EVar "n", EConst (CInt 2))) ) ) )
          )
      ; DLet
          ( false
          , PtVar "tmp"
          , EApp
              ( EApp (EApp (EVar "fib", EConst (CInt 15)), EConst (CInt 14))
              , EConst (CInt 3) ) )
      ; DLet
          ( false
          , PtVar "tmp"
          , ELet
              ( [ ( false
                  , PtVar "check"
                  , EFun
                      ( PtVar "f"
                      , EFun
                          ( PtVar "x"
                          , EIf
                              ( EBinOp (Eq, EApp (EVar "f", EVar "x"), EVar "x")
                              , EApp (EVar "f", EVar "x")
                              , EVar "x" ) ) ) )
                ]
              , EVar "check" ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"app fun and cons test"
    ~code:
      {|

      val ans = g ~100 (200 + 100);;

      val x = 1 :: 2 :: 432 :: [14];;

      val t = let in 3 + 3 end;;


    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "ans"
          , EApp
              ( EApp (EVar "g", EConst (CInt (-100)))
              , EBinOp (Add, EConst (CInt 200), EConst (CInt 100)) ) )
      ; DLet
          ( false
          , PtVar "x"
          , ECons
              ( EConst (CInt 1)
              , ECons
                  ( EConst (CInt 2)
                  , ECons (EConst (CInt 432), ECons (EConst (CInt 14), ENil)) ) ) )
      ; DLet (false, PtVar "t", ELet ([], EBinOp (Add, EConst (CInt 3), EConst (CInt 3))))
      ]
;;

let%test _ =
  test_parse
    ~label:"let in test"
    ~code:
      {|

      val tmp = let
        val t = 3
        fun x y z = y + z
      in
        t + x t 4
      end;;

      val t = (1, 2, 3);;

      val tmp = fn x => if x >= 2 then SOME x else NONE;;

      val tmp = fn x => (x >= 2, SOME x);;
    

    |}
    ~expected:
      [ DLet
          ( false
          , PtVar "tmp"
          , ELet
              ( [ false, PtVar "t", EConst (CInt 3)
                ; ( true
                  , PtVar "x"
                  , EFun (PtVar "y", EFun (PtVar "z", EBinOp (Add, EVar "y", EVar "z"))) )
                ]
              , EBinOp (Add, EVar "t", EApp (EApp (EVar "x", EVar "t"), EConst (CInt 4)))
              ) )
      ; DLet
          (false, PtVar "t", ETuple [ EConst (CInt 1); EConst (CInt 2); EConst (CInt 3) ])
      ; DLet
          ( false
          , PtVar "tmp"
          , EFun
              ( PtVar "x"
              , EIf (EBinOp (Geq, EVar "x", EConst (CInt 2)), ESome (EVar "x"), ENone) )
          )
      ; DLet
          ( false
          , PtVar "tmp"
          , EFun
              ( PtVar "x"
              , ETuple [ EBinOp (Geq, EVar "x", EConst (CInt 2)); ESome (EVar "x") ] ) )
      ]
;;
