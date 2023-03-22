(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

let parser str = Stdlib.Result.get_ok (parse_strings str)

(* Тесты парсера *)
let%test _ = parser "0" = XLiteral (BaseInt 0)
let%test _ = parser "true" = XLiteral (BaseBool true)
let%test _ = parser "xs" = XIdentifier "xs"
let%test _ = parser "~1" = XUnaryOp (Neg, XLiteral (BaseInt 1))

let%test _ =
  parser "(5 - 3)/2=(2 * 3 - 5)"
  = XBinaryOp
      ( Eq
      , XBinaryOp
          ( Div
          , XBinaryOp (Sub, XLiteral (BaseInt 5), XLiteral (BaseInt 3))
          , XLiteral (BaseInt 2) )
      , XBinaryOp
          ( Sub
          , XBinaryOp (Mult, XLiteral (BaseInt 2), XLiteral (BaseInt 3))
          , XLiteral (BaseInt 5) ) )
;;

let%test _ =
  parser "(\"some\", 123, 'a', true)"
  = XTuple
      [ XLiteral (BaseString "some")
      ; XLiteral (BaseInt 123)
      ; XLiteral (BaseChar 'a')
      ; XLiteral (BaseBool true)
      ]
;;

let%test _ =
  parser "[[11, 22], [\"aa\", \"bb\"], [true, false]]"
  = XList
      [ XList [ XLiteral (BaseInt 11); XLiteral (BaseInt 22) ]
      ; XList [ XLiteral (BaseString "aa"); XLiteral (BaseString "bb") ]
      ; XList [ XLiteral (BaseBool true); XLiteral (BaseBool false) ]
      ]
;;

let%test _ =
  parser "5 :: [6, 7]"
  = XConsList (XLiteral (BaseInt 5), XList [ XLiteral (BaseInt 6); XLiteral (BaseInt 7) ])
;;

let%test _ =
  parser "case xs of h :: tl => true | _ => false"
  = XCaseOf
      ( XIdentifier "xs"
      , [ XConsList (XIdentifier "h", XIdentifier "tl"), XLiteral (BaseBool true)
        ; XIdentifier "_", XLiteral (BaseBool false)
        ] )
;;

let%test _ =
  parser "let val x1 = 2 val x2 = 2 in x1 - x2 end"
  = XLetIn
      ( [ XValDec ("x1", XLiteral (BaseInt 2)); XValDec ("x2", XLiteral (BaseInt 2)) ]
      , XBinaryOp (Sub, XIdentifier "x1", XIdentifier "x2") )
;;

let%test _ =
  parser "func (x y)"
  = XApplication (XIdentifier "func", XApplication (XIdentifier "x", XIdentifier "y"))
;;

let%test _ = parser "val d = ~ d" = XValDec ("d", XUnaryOp (Neg, XIdentifier "d"))

let%test _ =
  parser "val rec factorial = fn n => if n <= 1 then 1 else n * factorial (n - 1)"
  = XValRecDec
      ( "factorial"
      , XArrowFun
          ( [ "n" ]
          , XIfThenElse
              ( XBinaryOp (LessEq, XIdentifier "n", XLiteral (BaseInt 1))
              , XLiteral (BaseInt 1)
              , XBinaryOp
                  ( Mult
                  , XIdentifier "n"
                  , XApplication
                      ( XIdentifier "factorial"
                      , XBinaryOp (Sub, XIdentifier "n", XLiteral (BaseInt 1)) ) ) ) ) )
;;

let%test _ =
  parser "fn x => fn y => x = y"
  = XArrowFun
      ([ "x" ], XArrowFun ([ "y" ], XBinaryOp (Eq, XIdentifier "x", XIdentifier "y")))
;;

let%test _ =
  parser "if true then false else true"
  = XIfThenElse
      (XLiteral (BaseBool true), XLiteral (BaseBool false), XLiteral (BaseBool true))
;;
