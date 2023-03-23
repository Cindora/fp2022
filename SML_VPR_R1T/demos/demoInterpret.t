Функция
  $ ./demoInterpret.exe << EOF
  > fn x => x
  fn

Арифметика
  $ ./demoInterpret.exe << EOF
  >  2 + 2 - 3 * (~6) / 2 
  13

Кортеж
  $ ./demoInterpret.exe << EOF
  > let val func = fn x => not x val y1 = 111 val y2 = 'c' val y3 = "str" in (y1, y2, y3, func true) end
  (111, 'c', "str", false)

Список
  $ ./demoInterpret.exe << EOF
  > let val f = fn x => fn xs => x::xs in f 'a' ['b', 'c'] end
  ['a', 'b', 'c']

Case of
  $ ./demoInterpret.exe << EOF
  > let val func = fn b => case b of
  > true => true
  > | _ => false in func false end
  false

Значение или 0
  $ ./demoInterpret.exe << EOF
  > let val x_or_null = fn x => fn y => let val get = (fn x => x)
  > val getget = (fn x => get get x)
  > in (case getget x of true => 0 | _ => (~y)) + y end
  > in x_or_null true 10 end
  10

Сумма отрицательных элементов массива
  $ ./demoInterpret.exe << EOF
  > let val rec sum_of_neg = fn xs => case xs of
  > [] => 0
  > | hd::tl => if hd < 0 then (hd + sum_of_neg tl) else (sum_of_neg tl)
  > in sum_of_neg [1, ~2, ~3, 4, 0] end
  -5

Факториал
  $ ./demoInterpret.exe << EOF
  > let val rec fix = (fn f => fn x => f (fix f) x) 
  > val fact = (fn self => fn n => if n = 0 then 1 else n * self (n - 1)) 
  > val f = (fn n => fix fact n) 
  > in f 5 end
  120

Фибоначчи
  $ ./demoInterpret.exe << EOF
  > let val rec fix = (fn f => fn x => f (fix f) x) 
  > val fibonacci = (fn self => fn n => if n <= 0 then 0 else if n = 1 then 1 else self (n-1) + self (n-2))
  > val f = (fn n => fix fibonacci n) 
  > in f 10 end 
  55

Использование подстановочного знака
  $ ./demoInterpret.exe << EOF
  > let val func = fn _ => true in func true end 
  true

Ошибки
  $ ./demoInterpret.exe << EOF
  > 1 / 0
  Деление на ноль.
  $ ./demoInterpret.exe << EOF
  > let val func = fn b => case b of true => false in func false end
  Pattern Matсhing не является исчерпывающим.
  $ ./demoInterpret.exe << EOF
  > let val fibonacci = fn n => if n <= 0 then 0 else if n = 1 then 1 else fibonacci \
  > (n-1) + fibonacci (n-2)
  > in fibonacci 10 end
  Ошибка присваивания значения.
  $ ./demoInterpret.exe << EOF
  > let val func = fn x => x * 2 in func _ end
  Ошибочное использование подстановочного знака.
