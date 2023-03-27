(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


Test 1
  $ ./interpretRun.exe <<-EOF
  >      public class Program {
  >             public static void Main() {
  >                   try
  >                   {
  >                     throw new Exception();
  >                     Console.WriteLine("Try");
  >                   }
  >                   catch
  >                   {
  >                     Console.WriteLine("Catch");
  >                   }
  >                   finally
  >                   {
  >                     Console.WriteLine("Finally");
  >                   }
  >             }
  >         }
  > EOF
  Catch
  Finally
  


Test 2
  $ ./interpretRun.exe <<-EOF
  >   public class Program {
  >           public static void Main() {
  >                   try
  >                   {
  >                     throw new Exception();
  >                     Console.WriteLine("Try");
  >                   }
  >                   catch (Exception)
  >                   {
  >                     Console.WriteLine("Catch");
  >                   }
  >             }
  >    }
  > EOF
  Catch
  


Test 3
  $ ./interpretRun.exe <<-EOF
  >        public class Program {
  >            static void Main()
  >            {
  >              Console.WriteLine(e());
  >            }
  >            static float e() {
  >                float num = 5.0;
  >                try 
  >                {
  >                    return num - 2.0;
  >                }
  >                catch 
  >                 {
  >                  Console.WriteLine(10);    
  >                 }
  >                finally 
  >                {
  >                    Console.WriteLine(num + 2.5);    
  >                }
  >            }
  >        }
  > EOF
  7.500000
  3.000000
  


Test 4
  $ ./interpretRun.exe <<-EOF
  >        public class Program {
  >            static void Main()
  >            { 
  >              int x = 1;
  >              float y = 6.4;
  >              Console.WriteLine(x + y);
  >           }
  >        }
  > EOF
  7.400000
  


Test 5
  $ ./interpretRun.exe <<-EOF
  >        public class Program {
  >            static void Main()
  >            {
  >             bool check = true;
  >              if (check == false){
  >                 Console.WriteLine(10);
  >                 }
  >              else
  >                {
  >                 Console.WriteLine(5);
  >                }
  >           }
  >        }
  > EOF
  5
  

Test 6
  $ ./interpretRun.exe <<-EOF
  >        public class Program {
  >            static void Main()
  >            {
  >             int i = 2;
  >              while(i != 6)
  >                 {
  >                 i = i + 2;
  >                 Console.WriteLine(i);
  >                 }
  >           }
  >        }
  > EOF
  4
  6
  


Test 7
  $ ./interpretRun.exe <<-EOF
  >            public class Program {
  >                 static void Main() {
  >                       for (int i = 1; i < 4; i++) {
  >                              Console.WriteLine(i*(i+2));
  >                       }
  >                 }
  >            }
  > EOF 
  3
  8
  15
  


Test 8
  $ ./interpretRun.exe <<-EOF
  >   public class Program {
  >           public static void Main() {
  >                     for (int i = 1, j = 1; i < 7; i++, j++)
  >                     {
  >                       Console.WriteLine(i * j);
  >                     }
  >             }
  >    }
  > EOF
  1
  4
  9
  16
  25
  36
  


Test 9
  $ ./interpretRun.exe <<-EOF
  >   public class Program {
  >           public static void Main() {
  >                     Console.WriteLine(6 > 4.9);
  >                     Console.WriteLine(10 % 3.0);
  >                     Console.WriteLine(10 + "10");
  >             }
  >    }
  > EOF
  false
  1.000000
  1010
  


