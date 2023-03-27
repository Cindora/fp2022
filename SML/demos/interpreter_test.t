% arithmetic operations test
  $ ./interpreter_test.exe << EOF
  > val x = 3 * 4 div 2 + 7;;
  > val y = 15 + 7 div 2;;
  > val result = x + y * x;;
  val x = 13: int
  val y = 18: int
  val result = 247: int
  _______

% pattern matching test
  $ ./interpreter_test.exe << EOF
  > val matching = fn [NONE, SOME x, SOME y] => 42 | SOME a :: b => 228 | _ => 1337;;
  > val test1 = matching [];;
  > val test2 = matching [SOME 5, NONE, NONE];;
  > val test3 = matching [NONE, SOME "xyz", SOME "abc"];;
  val matching = fn: 'e option list -> int
  val test1 = 1337: int
  val test2 = 228: int
  val test3 = 42: int
  _______

% val rec interpret test
  $ ./interpreter_test.exe << EOF
  > val rec timer = fn y => case y of
  >        | 0 => 1
  >        | _ => timer(y - 1)
  > ;;
  > val tmp = timer 17;;
  val timer = fn: int -> int
  val tmp = 1: int
  _______

% pattern matching inference test
  $ ./interpreter_test.exe << EOF
  > fun d x = case x of SOME t => t | NONE => ~1;;
  val d = fn: int option -> int
  _______

% binary operator inference test
  $ ./interpreter_test.exe << EOF
  > val e = fn x => (if x >= 2 then SOME x else NONE);;
  val e = fn: int -> int option
  _______

% typs test
  $ ./interpreter_test.exe << EOF
  > val int = 3;;
  > val bool = true;;
  > val string = "Hello World!";;
  > val list = [3, 1, 15, 7];;
  > val option = SOME (1, false);;
  > val tuple = (string, bool, int, list, option);;
  val int = 3: int
  val bool = true: bool
  val string = "Hello World!": string
  val list = [3, 1, 15, 7]: int list
  val option = SOME (1, false): (int * bool) option
  val tuple = ("Hello World!", true, 3, [3, 1, 15, 7], SOME (1, false)): (string * bool * int * int list * (int * bool) option)
  _______

% some constructors test
  $ ./interpreter_test.exe << EOF
  >  val x = 1 :: 2 :: 432 :: [14];;
  >  val t = let in 3 + 3 end;;
  val x = [1, 2, 432, 14]: int list
  val t = 6: int
  _______

% let in interpret test
  $ ./interpreter_test.exe << EOF
  >  val test = let
  >     val t = 3
  >     fun x y z = y + z
  >   in
  >     t + x t 4
  >   end;;
  val test = 10: int
  _______

% factorial test
  $ ./interpreter_test.exe << EOF
  > fun fact n = if n = 0 then 1 else n * fact (n-1);;
  > val fact5 = fact 5;;
  > val fact10 = fact 10;;
  val fact = fn: int -> int
  val fact5 = 120: int
  val fact10 = 3628800: int
  _______

% factorial accumulate test
  $ ./interpreter_test.exe << EOF
  > fun factacc n acc = if n = 0 then acc else factacc (n - 1) acc * n;;
  > val factacc5 = factacc 5 1;;
  > val factacc10 = factacc 10 1;;
  val factacc = fn: int -> int -> int
  val factacc5 = 120: int
  val factacc10 = 3628800: int
  _______

% fibonacci test
  $ ./interpreter_test.exe << EOF
  > fun fib n = if n <= 1 then 1 else fib(n - 1) + fib(n - 2);;
  > val fib15 = fib 15;;
  val fib = fn: int -> int
  val fib15 = 987: int
  _______

% list last element function test without error checking
  $ ./interpreter_test.exe << EOF
  > fun last list = case list of [x] => x | a :: b => last b;;
  > val res = last [1, 7, 5, 11, 15, 7];;
  val last = fn: 'c list -> 'c
  val res = 7: int
  _______

% list at index element function test without error checking
  $ ./interpreter_test.exe << EOF
  > fun at list index = case list of a :: b => if index = 0 then a else at b (index - 1);;
  > val res = at [1, 7, 5, 11, 15, 7] 4;;
  val at = fn: 'd list -> int -> 'd
  val res = 15: int
  _______

% equality type test
  $ ./interpreter_test.exe << EOF
  > val x = SOME 15;;
  > val y = x;;
  > val check = x = y;;
  val x = SOME 15: int option
  val y = SOME 15: int option
  val check = true: bool
  _______

% not equality type test
  $ ./interpreter_test.exe << EOF
  > val x = fn x => x;;
  > val y = x;;
  > val check = x = y;;
  Elaboration failed: Type clash. Binary Eq operator cannot take arguments of type fn and fn
  _______

% unary operator inference test
  $ ./interpreter_test.exe << EOF
  > val r = fn x => not x;;
  > val minus = fn x => ~x;;
  val r = fn: bool -> bool
  val minus = fn: int -> int
  _______

% option none equality test
  $ ./interpreter_test.exe << EOF
  > val f = fn t => if t = 4 then NONE else SOME 5;;
  > val ft = fn t => if t = 4 then NONE else SOME "5";;
  > val check = f 4 = ft 4;;
  Elaboration failed: Rules disagree on type: Cannot merge string and int
  _______

% option some equality test
  $ ./interpreter_test.exe << EOF
  > val f = fn t => if t = 4 then NONE else SOME 5;;
  > val ft = fn t => if t = 4 then NONE else SOME "5";;
  > val check = f 5 = ft 5;;
  Elaboration failed: Type clash. Binary Eq operator cannot take arguments of type int and string
  _______

% wildcard lambda argument test
  $ ./interpreter_test.exe << EOF
  > val r = fn _ => fn x => fn _ => 2 * x;;
  > val t = r true 4 "abc";;
  val r = fn: 'a -> int -> 'c -> int
  val t = 8: int
  _______

% pattern matching, asteriks, equality inference test
  $ ./interpreter_test.exe << EOF
  > fun id z = z;;
  > fun f x y a b = x >= y andalso a < b;;
  > val tmp = fn x => fn y => case y of (a, b) => (a * 2, b = "xyz");;
  val id = fn: 'b -> 'b
  val f = fn: int -> int -> int -> int -> bool
  val tmp = fn: 'a -> (int * string) -> (int * bool)
  _______

% types disagree test
  $ ./interpreter_test.exe << EOF
  > val tmp = fn x => fn y => x = y orelse y + x;;
  Elaboration failed: Rules disagree on type: Cannot merge int and bool
  _______

% value restriction test
  $ ./interpreter_test.exe << EOF
  > val before_applying = let in (fn x => fn z => (x, z)) end;;
  > val tmp = before_applying 4;;
  > val after_applying = before_applying;;
  val before_applying = fn: '~A -> '~B -> ('~A * '~B)
  val tmp = fn: '~B -> (int * '~B)
  val after_applying = fn: int -> '~B -> (int * '~B)
  _______

% if else fn test inference
  $ ./interpreter_test.exe << EOF
  > val b = if 4 = 4 then fn x => fn y => x = y else fn a => fn b => a orelse b;;
  val b = fn: bool -> bool -> bool
  _______

% value restriction error test
  $ ./interpreter_test.exe << EOF
  > val b = if 4 = 4 then fn x => fn y => [x, y] else fn a => fn b => [a, b];;
  > val x = b 4;;
  > val y = b true;;
  Elaboration failed: Rules disagree on type: Cannot merge bool and int
  _______

% not value restriction test
  $ ./interpreter_test.exe << EOF
  > val z = fn x => (case x of 1 => (fn t => t) | _ => (fn t => t));;
  val z = fn: int -> 'b -> 'b
  _______


% value restriction test 2
  $ ./interpreter_test.exe << EOF
  > val before_applying = if 4 = 4 then fn x => fn y => [x, y] else fn x => fn y => [y, x];;
  > val tmp = before_applying "abc";;
  > val after_applying = before_applying;;
  val before_applying = fn: '~D -> '~D -> '~D list
  val tmp = fn: string -> string list
  val after_applying = fn: string -> string -> string list
  _______

% inference + interpreter test
  $ ./interpreter_test.exe << EOF
  > val x = fn t => if t >= 2 then SOME t else NONE;;
  > val y = [(x 5), (x 1), (x 3), (x 18)];;
  val x = fn: int -> int option
  val y = [SOME 5, NONE, SOME 3, SOME 18]: int option list
  _______

% equality types inference test
  $ ./interpreter_test.exe << EOF
  > fun f x y = if x = y then fn t => t else fn t => t;;
  val f = fn: ''d -> ''d -> 'e -> 'e
  _______

% value restriction and equality types test
  $ ./interpreter_test.exe << EOF
  > val function =
  >    let
  >      val check = fn f => fn x => if f x = x then f x else x
  >    in
  >      check
  >    end;;
  val function = fn: (''~E -> ''~E) -> ''~E -> ''~E
  _______

% equality types and type inference test
  $ ./interpreter_test.exe << EOF
  > fun f x y = if x = y then (NONE, SOME x) else (NONE, NONE);;
  val f = fn: ''d -> ''d -> ('e option * ''d option)
  _______

% equality type error test
  $ ./interpreter_test.exe << EOF
  > fun f x y = if x = y then (NONE, SOME x) else (NONE, NONE);;
  > val result = f (fn x => 2 * x);;
  Elaboration failed: Rules disagree on type: Cannot merge int -> int and ''d
  _______

% fix factorial test
  $ ./interpreter_test.exe << EOF
  > val test = let
  >     fun fix f x = f (fix f) x 
  >     fun fact self n = if n = 0 then 1 else n * self (n - 1)
  >     val function = fn n => fix fact n
  > in
  >     function 6
  > end;;
  val test = 720: int
  _______


% equality types and value restriction test inference test by Kakadu
  $ ./interpreter_test.exe << EOF
  > val tmp = let
  >                 val eqq = fn x => fn y => x=y
  >                 val foo = fn eq => fn x => fn y => eq x y
  > in  
  >                 fn eta => foo eqq eta
  > end;;
  > 
  val tmp = fn: ''~D -> ''~D -> bool
  _______
