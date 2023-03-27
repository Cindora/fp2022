(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Opal

let ( let* ) = ( >>= )
let parsing p s = parse p (LazyStream.of_string s)
let const = token "const" >> return Const
let parens = between (token "(") (token ")")
let braces = between (token "{") (token "}")

let reserved =
  [ "true"
  ; "false"
  ; "public"
  ; "static"
  ; "const"
  ; "override"
  ; "if"
  ; "else"
  ; "while"
  ; "try"
  ; "catch"
  ; "finally"
  ; "when"
  ; "void"
  ; "string"
  ; "char"
  ; "Console"
  ; "namespace"
  ; "using"
  ; "int"
  ; "bool"
  ; "for"
  ; "null"
  ; "new"
  ; "return"
  ; "break"
  ; "continue"
  ; "class"
  ]
;;

let modifiers =
  many
    (choice
       [ token "public" >> return Public
       ; token "static" >> return Static
       ; token "const" >> return Const
       ; token "override" >> return Override
       ])
;;

module Expr = struct
  open Ast

  let number = spaces >> many1 digit => implode
  let int_v = number => int_of_string

  let%test _ = parsing int_v "4" = Some 4

  let get_int =
    let* i = int_v in
    return (Value (Int' i))
  ;;

  let check_next f s =
    match any s with
    | Some (x, _) when f (Some x) -> (return ()) s
    | None when f None -> (return ()) s
    | _ -> mzero s
  ;;

  let float_v =
    spaces
    >> let* int_part = many1 digit in
       let* float_part =
         exactly '.'
         >> check_next (function
              | Some x -> x != '.'
              | _ -> true)
         >> many1 digit
       in
       return (float_of_string (implode (int_part @ ('.' :: float_part))))
  ;;

  let%test _ = parsing float_v "34.5" = Some 34.5

  let get_float =
    let* f = float_v in
    return (Value (Float' f))
  ;;

  let get_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\""
    >> let* list = many (satisfy (fun x -> x <> '\"')) in
       token "\"" >> return (Value (String' (string_of_chars list)))
  ;;

  let get_bool =
    choice
      [ token "false"
        >> return (Value (Bool' false))
        <|> (token "true" >> return (Value (Bool' true)))
      ]
  ;;

  let binop_plus = token "+" >> return (fun x y -> Binop (Plus, x, y))
  let binop_minus = token "-" >> return (fun x y -> Binop (Minus, x, y))
  let binop_multiply = token "*" >> return (fun x y -> Binop (Multiply, x, y))
  let binop_divide = token "/" >> return (fun x y -> Binop (Divide, x, y))
  let binop_mod = token "%" >> return (fun x y -> Binop (Mod, x, y))
  let compare_equal = token "==" >> return (fun x y -> Compare (Equal, x, y))
  let compare_not_equal = token "!=" >> return (fun x y -> Compare (NotEqual, x, y))
  let compare_less = token "<" >> return (fun x y -> Compare (Less, x, y))
  let compare_more = token ">" >> return (fun x y -> Compare (More, x, y))

  let compare_less_or_equal =
    token "<=" >> return (fun x y -> Compare (LessOrEqual, x, y))
  ;;

  let compare_more_or_equal =
    token ">=" >> return (fun x y -> Compare (MoreOrEqual, x, y))
  ;;

  let null = token "null" >> return Null
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))

  let ident =
    spaces
    >> let* x = letter <~> many alpha_num => implode in
       match x with
       | x when List.mem x reserved -> mzero
       | x -> return x
  ;;

  let get_variable = ident => fun x -> Variable x
  let atomic = get_float <|> get_int <|> get_variable <|> get_string <|> get_bool <|> null

  let%test _ = parsing atomic "    12.3   " = Some (Value (Float' 12.3))
  let%test _ = parsing atomic "    123  " = Some (Value (Int' 123))
  let%test _ = parsing atomic " x " = Some (Variable "x")
  let%test _ = parsing atomic "\"string...\"" = Some (Value (String' "string..."))
  let%test _ = parsing atomic "true" = Some (Value (Bool' true))
  let%test _ = parsing atomic "  null " = Some Null

  let define_type =
    choice
      [ token "float" >> return Float
      ; token "int" >> return Int
      ; token "string" >> return String
      ; token "void" >> return Void
      ; token "bool" >> return Bool
      ; (let* class_name = ident in
         return (Class class_name))
      ]
  ;;

  let%test _ = parsing define_type "void" = Some Void
  let%test _ = parsing define_type "string" = Some String
  let%test _ = parsing define_type "Cat" = Some (Class "Cat")

  let rec expr input = and_or_expr input
  and and_or_expr input = (chainl1 compare_expr (and_op <|> or_op)) input

  and compare_expr input =
    (chainl1
       binop_expr
       (compare_less_or_equal
       <|> compare_more_or_equal
       <|> compare_less
       <|> compare_more
       <|> compare_equal
       <|> compare_not_equal))
      input

  and binop_expr input =
    (chainl1
       unary_expr
       (binop_multiply <|> binop_divide <|> binop_mod <|> binop_plus <|> binop_minus))
      input

  and unary_expr input =
    choice
      [ (token "!"
        >> let* x = lexeme primar_expr in
           return (Not x))
      ; (token "-"
        >> let* x = lexeme primar_expr in
           return (Binop (Minus, Value (Int' 0), x)))
      ; (let* x = lexeme primar_expr in
         token "++" >> return (IncrementPost x))
      ; (let* x = lexeme primar_expr in
         token "--" >> return (DecrementPost x))
      ; primar_expr
      ]
      input

  and primar_expr input =
    (init_instance <|> assign <|> call_method <|> parens expr <|> atomic) input

  and split_by_comma input = sep_by expr (token ",") input

  and call_method input =
    (let* name = ident in
     token "("
     >> let* args_list = split_by_comma in
        token ")" >> return (CallMethod (name, args_list)))
      input

  and init_instance input =
    (token "new"
    >> let* name = ident in
       token "("
       >> let* args_list = split_by_comma in
          token ")" >> return (ClassCreate (name, args_list)))
      input

  and assign input =
    let parse_left = call_method <|> get_variable in
    (let* left = parse_left in
     token "="
     >> let* right = expr in
        return (Assign (left, right)))
      input
  ;;

  let%test _ = parsing expr "x = y " = Some (Assign (Variable "x", Variable "y"))

  let%test _ =
    parsing expr "1 + 2.5" = Some (Binop (Plus, Value (Int' 1), Value (Float' 2.5)))
  ;;

  let%test _ =
    parsing expr "check = false" = Some (Assign (Variable "check", Value (Bool' false)))
  ;;
end

module Statement = struct
  open Expr

  let rec statements input =
    choice
      [ continue
      ; break
      ; parse_expr
      ; return_statement
      ; if_statement
      ; while_statement
      ; throw_statement
      ; variale_declare
      ; for_statement
      ; try_statement
      ; block_statement
      ; print_statement
      ]
      input

  and if_statement input =
    (token "if"
    >> let* condition = parens expr in
       let* if_body = statements in
       choice
         [ (token "else"
           >> let* else_body = statements in
              return (If (condition, if_body, Some else_body)))
         ; return (If (condition, if_body, None))
         ])
      input

  and block_statement input =
    (let* stats = braces (sep_by statements spaces) in
     return (Block stats))
      input

  and while_statement input =
    (token "while"
    >> let* condition = parens expr in
       let* while_body = statements in
       return (While (condition, while_body)))
      input

  and variale_declare input =
    let helper =
      let* var_name = ident in
      token "="
      >> let* var_value = expr in
         return (var_name, Some var_value) <|> return (var_name, None)
    in
    choice
      [ (let* const = const in
         let* var_type = define_type in
         let* var_pair = sep_by1 helper (token ",") in
         token ";" >> return (VariableDeclare (Some const, var_type, var_pair)))
      ; (let* var_type = define_type in
         let* var_pair = sep_by1 helper (token ",") in
         token ";" >> return (VariableDeclare (None, var_type, var_pair)))
      ]
      input

  and for_statement input =
    (token "for"
    >> token "("
    >> let* declare =
         choice
           [ (let* var = variale_declare in
              return (Some var))
           ; token ";" >> return None
           ]
       in
       let* condition =
         choice
           [ (let* expr = expr in
              token ";" >> return (Some expr))
           ; token ";" >> return None
           ]
       in
       let* after = sep_by expr (token ",") in
       token ")"
       >> let* body = statements in
          return (For (declare, condition, after, body)))
      input

  and throw_statement input =
    (token "throw"
    >> let* throw_expr = expr in
       token ";" >> return (Throw throw_expr))
      input

  and parse_expr input =
    (let* exprs = expr in
     token ";" >> return (Expr exprs))
      input

  and return_statement input =
    (token "return"
    >> choice
         [ (skip_many1 space
           >> let* result = expr in
              token ";" >> return (Return (Some result)))
         ; token ";" >> return (Return None)
         ])
      input

  and continue input = (token "continue" >> token ";" >> return Continue) input
  and break input = (token "break" >> token ";" >> return Break) input

  and try_statement input =
    let catch =
      token "catch"
      >> choice
           [ (token "("
             >> let* excep_type = define_type in
                token ")"
                >> let* catch_block = statements in
                   return (Some excep_type, catch_block))
           ; (let* catch_block = statements in
              return (None, catch_block))
           ]
    in
    (token "try"
    >> let* try_statement = statements in
       let* catch_list = many catch in
       match catch_list with
       | [] ->
         token "finally"
         >> let* finally_block = statements in
            return (Try (try_statement, catch_list, Some finally_block))
       | _ ->
         choice
           [ (token "finally"
             >> let* finally_block = statements in
                return (Try (try_statement, catch_list, Some finally_block)))
           ; return (Try (try_statement, catch_list, None))
           ])
      input

  and print_statement input =
    (token "Console.WriteLine("
    >> let* print_expr = expr in
       token ");" >> return (Print print_expr))
      input
  ;;

  let%test _ =
    parsing return_statement "return x >= y;"
    = Some (Return (Some (Compare (MoreOrEqual, Variable "x", Variable "y"))))
  ;;
end

module Fields_and_classes = struct
  open Expr
  open Statement

  let get_params =
    let* type' = define_type in
    let* name = ident in
    return (type', name)
  ;;

  let class_method =
    let* type' = define_type in
    let* name = ident in
    token "("
    >> let* params_list = sep_by get_params (token ",") in
       token ")"
       >> let* block = block_statement in
          return (Method (type', name, params_list, block))
  ;;

  let field =
    let helper =
      let* name = ident in
      token "="
      >> (let* value = expr in
          return (name, Some value))
      <|> return (name, None)
    in
    let* type' = define_type in
    let* var_list = sep_by helper (token ",") in
    token ";" >> return (VariableField (type', var_list))
  ;;

  let class_elem =
    let* modifier = modifiers in
    let* class_elem = field <|> class_method in
    return (modifier, class_elem)
  ;;

  let parse_class =
    let* modifier = modifiers in
    token "class"
    >> let* name = ident in
       let* parent' =
         choice
           [ (token ":"
             >> let* parent = ident in
                return (Some parent))
           ; return None
           ]
       in
       token "{"
       >> let* class_elem = sep_by class_elem spaces in
          token "}" >> return (ClassDec (modifier, name, parent', class_elem))
  ;;

  let parser = many parse_class

  let%test _ =
    parsing field {| int x = 10; |}
    = Some (VariableField (Int, [ "x", Some (Value (Int' 10)) ]))
  ;;

  let%test _ = parsing field {| int x; |} = Some (VariableField (Int, [ "x", None ]))

  let%test _ =
    parsing
      parse_class
      {|
               public class Cat
               {
                 float x = 10.7; 
                 public float Example()
                 {  
                  return x;
                 }
               }
           |}
    = Some
        (ClassDec
           ( [ Public ]
           , "Cat"
           , None
           , [ [], VariableField (Float, [ "x", Some (Value (Float' 10.7)) ])
             ; ( [ Public ]
               , Method (Float, "Example", [], Block [ Return (Some (Variable "x")) ]) )
             ] ))
  ;;

  let%test _ =
    parsing
      parse_class
      {|
               public class Exception
               {
                 public string Message;
                 public string ToString()
                 {  
                  return Message;
                 }
               }
           |}
    = Some
        (ClassDec
           ( [ Public ]
           , "Exception"
           , None
           , [ [ Public ], VariableField (String, [ "Message", None ])
             ; ( [ Public ]
               , Method
                   (String, "ToString", [], Block [ Return (Some (Variable "Message")) ])
               )
             ] ))
  ;;
end
