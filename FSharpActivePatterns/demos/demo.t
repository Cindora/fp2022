Constants and types
  $ ./demo.exe <<- EOF
  > 777;;
  (VInt 777)
  $ ./demo.exe <<- EOF
  > "Thunder";;
  (VString "Thunder")
  $ ./demo.exe <<- EOF
  > true;;
  (VBool true)
  $ ./demo.exe <<- EOF
  > [];;
  VNil
  $ ./demo.exe <<- EOF
  > ();;
  VUnit
  $ ./demo.exe <<- EOF
  > [7;7;7];;
  (VList [(VInt 7); (VInt 7); (VInt 7)])
  $ ./demo.exe <<- EOF
  > (777, "Somebody once told me...", false);;
  (VTuple [(VInt 777); (VString "Somebody once told me..."); (VBool false)])
  $ ./demo.exe <<- EOF
  > Some 1;;
  (VSome (VInt 1))
  $ ./demo.exe <<- EOF
  > None;;
  VNone
Arithmetic operations
  $ ./demo.exe <<- EOF
  > 700 + 70 + 7;;
  (VInt 777)
  $ ./demo.exe <<- EOF
  > 123 - 20 - 3;;
  (VInt 100)
  $ ./demo.exe <<- EOF
  > 7 * 7 * 7;;
  (VInt 343)
  $ ./demo.exe <<- EOF
  > 999 / 333 / 1;;
  (VInt 3)
  $ ./demo.exe <<- EOF
  > 777 % 2;;
  (VInt 1)
  $ ./demo.exe <<- EOF
  > 777 < 999;;
  (VBool true)
  $ ./demo.exe <<- EOF
  > Some (777) >= None;;
  (VBool true)
  $ ./demo.exe <<- EOF
  > "kakadu" = "kakadu";;
  (VBool true)
  $ ./demo.exe <<- EOF
  > (1, 2, 3) <> (1, 2, 3);;
  (VBool false)
  $ ./demo.exe <<- EOF
  > 1 > 2 && 4 = 4;;
  (VBool false)
  $ ./demo.exe <<- EOF
  > 777 <> 1000 || None <> None;;
  (VBool true)
  $ ./demo.exe <<- EOF
  > 1 :: 2 :: [];;
  (VList [(VInt 1); (VInt 2)])
  $ ./demo.exe <<- EOF
  > 7 :: [7;7];;
  (VList [(VInt 7); (VInt 7); (VInt 7)])
  $ ./demo.exe <<- EOF
  > 1 + 2 + 3 * 7 - (5 + 4) % 2 + 100 * (25 - 1) / 4 > 10 || false && 10 < 1 || 1 :: [2] = [1;2];;
  (VBool true)
Conditions and pattern matching
  $ ./demo.exe <<- EOF
  > if None = None then Some None else None;;
  (VSome VNone)
  $ ./demo.exe <<- EOF
  > if "meow" = "cat" then "meow" else "woof";;
  (VString "woof")
  $ ./demo.exe <<- EOF
  > match Some 1 with
  >   | Some x -> x
  >   | None -> 0;;
  (VInt 1)
  $ ./demo.exe <<- EOF
  > match 777 with
  > | 0 -> "Zero"
  > | _ -> "Good luck";;
  (VString "Good luck")
Lambda functions
  $ ./demo.exe <<- EOF
  > fun x -> x;;
  (VFun ((PVar "x"), (EVar "x"), []))
  $ ./demo.exe <<- EOF
  > fun x y z -> x + y + z;;
  (VFun ((PVar "x"),
     (EFun ((PVar "y"),
        (EFun ((PVar "z"),
           (EBinOp (Add, (EBinOp (Add, (EVar "x"), (EVar "y"))), (EVar "z")))))
        )),
     []))
  $ ./demo.exe <<- EOF
  > fun x -> fun y -> x * y;;
  (VFun ((PVar "x"),
     (EFun ((PVar "y"), (EBinOp (Mul, (EVar "x"), (EVar "y"))))), []))
Bindings
  $ ./demo.exe <<- EOF
  > let x = 777;;
  (VInt 777)
  $ ./demo.exe <<- EOF
  > let f x = x + 777;;
  (VFun ((PVar "x"), (EBinOp (Add, (EVar "x"), (EConst (CInt 777)))), []))
  $ ./demo.exe <<- EOF
  > let rec f x = if x < 50 then x + 50 else f (x - 50);;
  (VFunRec ("f",
     (VFun ((PVar "x"),
        (EIf ((EBinOp (Less, (EVar "x"), (EConst (CInt 50)))),
           (EBinOp (Add, (EVar "x"), (EConst (CInt 50)))),
           (EApp ((EVar "f"), (EBinOp (Sub, (EVar "x"), (EConst (CInt 50))))))
           )),
        []))
     ))
  $ ./demo.exe <<- EOF
  > let sum x = fun y -> x + y in sum (sum 7 70) 700;;
  (VInt 777)
Calling a variable name
  $ ./demo.exe <<- EOF
  > let x = 777;; x;;
  (VInt 777)
  $ ./demo.exe <<- EOF
  > let f = fun x -> x;; f;;
  (VFun ((PVar "x"), (EVar "x"), []))
Apply
  $ ./demo.exe <<- EOF
  > let app x f = f x;; app 100 (fun x -> ((x - 1) / 9 * 10 + 1) * 7);;
  (VInt 777)
  $ ./demo.exe <<- EOF
  > (fun x y -> x / y) 100 5;;
  (VInt 20)
Factorial
  $ ./demo.exe <<- EOF
  > let rec factorial n = if n <= 1 then 1 else n * (factorial (n - 1))
  > in factorial 20;;
  (VInt 2432902008176640000)
Fibonacci
  $ ./demo.exe <<- EOF
  > let rec fibonacci n = if n < 2 then 1 else fibonacci (n - 1) + fibonacci (n - 2);;
  > let _ = fibonacci 20;;
  (VInt 10946)
Active patterns
  $ ./demo.exe <<- EOF
  > let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd;;
  > let test_number input =
  >   match input with
  >     | Even -> "This number is even"
  >     | Odd -> "This number is odd";;
  > test_number 7;;
  (VString "This number is odd")
  $ ./demo.exe <<- EOF
  > let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd;;
  > let test_number input =
  >   match input with
  >     | Even -> "This number is even"
  >     | Odd -> "This number is odd";;
  > test_number 32;;
  (VString "This number is even")
  $ ./demo.exe <<- EOF
  > let (|Default|) onNone value =
  >   match value with
  >     | None -> onNone
  >     | Some e -> e;;
  > let greet (Default "random citizen" name) = name;;
  > greet None;;
  (VString "random citizen")
  $ ./demo.exe <<- EOF
  > let (|Default|) onNone value =
  >   match value with
  >     | None -> onNone
  >     | Some e -> e;;
  > let greet (Default "random citizen" name) = name;;
  > greet (Some "George");;
  (VString "George")
  $ ./demo.exe <<- EOF
  > let (| Foo|_|) s x = if x = s then Some Foo else None;;
  > match 777 with
  >   | Foo 777 -> "You win :)"
  >   | _ -> "You lose :(";;
  (VString "You win :)")
