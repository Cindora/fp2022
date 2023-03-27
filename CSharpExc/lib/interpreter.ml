(** Copyright 2021-2022, Polin Badreeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf

module type MONADERROR = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let ( >> ) x f = x >>= fun _ -> f
  let error = Result.error
end

module Interpreter (M : MONADERROR) = struct
  open M
  open Types_setup

  let ( let* ) = ( >>= )

  type var =
    { var_type : types
    ; var_key : key_setup
    ; var_value : values
    ; is_const : bool
    ; assignment_count : int
    ; level : int
    }
  [@@deriving show { with_path = false }]

  type flag =
    | WasBreak
    | WasContinue
    | WasReturn
    | WasThrown
    | NoFlag
  [@@deriving show { with_path = false }]

  and context =
    { var_map : var KeyMap.t
    ; method_type : types
    ; last_result : values
    ; runtime_flag : flag
    ; count_cycles : int
    ; level : int
    ; increment : key_setup list
    ; decrement : key_setup list
    }
  [@@deriving show { with_path = false }]

  let init_contex var_map =
    return
      { var_map
      ; method_type = Void
      ; last_result = Void'
      ; runtime_flag = NoFlag
      ; count_cycles = 0
      ; level = 0
      ; increment = []
      ; decrement = []
      }
  ;;

  let find_main_class (class_map : class_setup KeyMap.t) =
    let class_seq = KeyMap.to_seq class_map in
    let rec iter_classes (class_s : (key_setup * class_setup) Seq.t) =
      match class_s () with
      | Seq.Nil -> error (str_of_error NoMain)
      | Seq.Cons ((_, x), xs) ->
        (match KeyMap.find_opt "Main" x.method_map with
         | None -> iter_classes xs
         | Some _ -> return x)
    in
    iter_classes class_seq
  ;;

  let add_var context var =
    { context with var_map = KeyMap.add var.var_key var context.var_map }
  ;;

  let replace_var context var_key var =
    let var_map = KeyMap.remove var_key context.var_map in
    let var_map = KeyMap.add var_key var var_map in
    { context with var_map }
  ;;

  let remove_key_increment context var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key i =
      match i with
      | [] -> i
      | x :: xs ->
        (match check_var_key x with
         | None -> change_var_key xs
         | Some el -> el :: change_var_key xs)
    in
    { context with increment = change_var_key context.increment }
  ;;

  let remove_key_decrement context var_key =
    let check_var_key x = if var_key = x then None else Some x in
    let rec change_var_key d =
      match d with
      | [] -> d
      | x :: xs ->
        (match check_var_key x with
         | None -> change_var_key xs
         | Some el -> el :: change_var_key xs)
    in
    { context with decrement = change_var_key context.decrement }
  ;;

  let find_method_monad meth_map meth_key =
    match KeyMap.find_opt meth_key meth_map with
    | None -> error (str_of_error (NoFindMethod meth_key))
    | Some method_t -> return method_t
  ;;

  let rec check_expr_type cur_expr context class_map =
    match cur_expr with
    | Binop (Plus, l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Float | Int ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Float -> return Float
            | Int -> return Int
            | String -> return String
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | String ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Int | String | Float -> return String
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ -> error (str_of_error IncorrectType)
      in
      l_arg l_type
    | Binop (Minus, l, r)
    | Binop (Divide, l, r)
    | Binop (Mod, l, r)
    | Binop (Multiply, l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Float ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Float -> return Float
            | Int -> return Float
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | Int ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Float -> return Float
            | Int -> return Int
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ -> error (str_of_error IncorrectType)
      in
      l_arg l_type
    | And (l, r) | Or (l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Bool ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Bool -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ -> error (str_of_error IncorrectType)
      in
      l_arg l_type
    | Not value ->
      let* value_type = check_expr_type value context class_map in
      let pm = function
        | Bool -> return Bool
        | _ -> error (str_of_error IncorrectType)
      in
      pm value_type
    | IncrementPost value | DecrementPost value ->
      let* value_type = check_expr_type value context class_map in
      let pm = function
        | Int -> return Int
        | _ -> error (str_of_error IncorrectType)
      in
      pm value_type
    | Compare (Less, l, r)
    | Compare (More, l, r)
    | Compare (LessOrEqual, l, r)
    | Compare (MoreOrEqual, l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Float ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Float -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | Int ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Int -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ -> error (str_of_error IncorrectType)
      in
      l_arg l_type
    | Compare (Equal, l, r) | Compare (NotEqual, l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Float ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Float -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | Int ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Int -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | String ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | String -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | Bool ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Bool -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | Class _ ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Class _ -> return Bool
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ -> error (str_of_error IncorrectType)
      in
      l_arg l_type
    | Null -> return (Class "null")
    | CallMethod (method_key, _) ->
      let* curr_class = find_main_class class_map in
      let* mthd = find_method_monad curr_class.method_map method_key in
      return mthd.method_type
    | ClassCreate (class_name, _) ->
      (match KeyMap.find_opt class_name class_map with
       | None -> error (str_of_error (NoFindClass class_name))
       | Some _ -> return (Class class_name))
    | Variable var_key ->
      (match KeyMap.find_opt var_key context.var_map with
       | None -> error (str_of_error (NoVariable var_key))
       | Some var -> return var.var_type)
    | Value value ->
      (match value with
       | Float' _ -> return Float
       | Int' _ -> return Int
       | Bool' _ -> return Bool
       | String' _ -> return String
       | Class' ObjectNull -> return (Class "null")
       | Class' (ObjectRef (class_key, _)) -> return (Class class_key)
       | _ -> error (str_of_error IncorrectType))
    | Assign (l, r) ->
      let* l_type = check_expr_type l context class_map in
      let l_arg = function
        | Void -> error (str_of_error IncorrectType)
        | Class l_key ->
          let* r_type = check_expr_type r context class_map in
          let r_arg = function
            | Class "null" -> return (Class l_key)
            | Class right_key -> return (Class right_key)
            | _ -> error (str_of_error IncorrectType)
          in
          r_arg r_type
        | _ ->
          let* r_type = check_expr_type r context class_map in
          if l_type = r_type then return r_type else error (str_of_error IncorrectType)
      in
      l_arg l_type
  ;;

  let expr_in_statement = function
    | IncrementPost _ | DecrementPost _ | CallMethod (_, _) | Assign (_, _) -> true
    | _ -> false
  ;;

  let rec eval_statement statement in_context class_map =
    match statement with
    | Expr expr ->
      let* in_context = eval_inc_dec in_context in
      if expr_in_statement expr
      then
        let* new_context = eval_expr expr in_context class_map in
        return new_context
      else error (str_of_error IncorrectType)
    | Block stat_list ->
      let rec eval_statement_bl : statement list -> context -> context M.t =
       fun stl context ->
        match stl with
        | [] -> return context
        | st :: tail ->
          (match st with
           | (Break | Continue | Return _) when tail <> [] ->
             error (str_of_error BrokeDown)
           | _ when context.count_cycles >= 1 && context.runtime_flag = WasBreak ->
             return context
           | _ when context.count_cycles >= 1 && context.runtime_flag = WasContinue ->
             return context
           | _ when context.runtime_flag = WasReturn -> return context
           | _ when context.runtime_flag = WasThrown -> return context
           | _ ->
             let* head_context = eval_statement st context class_map in
             eval_statement_bl tail head_context)
      in
      let* new_context = eval_statement_bl stat_list in_context in
      let* new_context = eval_inc_dec new_context in
      return new_context
    | If (expr, then_stat, else_stat_opt) ->
      let* if_context = eval_expr expr in_context class_map in
      let* if_context = eval_inc_dec if_context in
      (match if_context.last_result with
       | Bool' true ->
         (match then_stat with
          | Block _ ->
            let* in_context =
              eval_statement
                then_stat
                { if_context with level = if_context.level + 1 }
                class_map
            in
            return { in_context with level = in_context.level - 1 }
          | _ -> eval_statement then_stat if_context class_map)
       | Bool' false ->
         (match else_stat_opt with
          | Some (Block _ as else_stat) ->
            let* else_context =
              eval_statement
                else_stat
                { if_context with level = if_context.level + 1 }
                class_map
            in
            return { else_context with level = else_context.level - 1 }
          | Some else_stat -> eval_statement else_stat if_context class_map
          | None -> return in_context)
       | _ -> error (str_of_error IncorrectType))
    | While (expr, stat) ->
      let rec eval_loop loop_stat context =
        if context.runtime_flag = WasBreak
        then (
          match loop_stat with
          | Block _ ->
            return
              { context with
                runtime_flag = NoFlag
              ; count_cycles = context.count_cycles - 1
              ; level = context.level - 1
              }
          | _ ->
            return
              { context with
                runtime_flag = NoFlag
              ; count_cycles = context.count_cycles - 1
              })
        else
          let* new_context = eval_expr expr context class_map in
          let* new_context = eval_inc_dec new_context in
          match new_context.last_result with
          | Bool' false ->
            (match loop_stat with
             | Block _ ->
               return
                 { new_context with
                   count_cycles = context.count_cycles - 1
                 ; level = context.level - 1
                 }
             | _ -> return { new_context with count_cycles = context.count_cycles - 1 })
          | Bool' true ->
            let* loop_context = eval_statement loop_stat new_context class_map in
            (match loop_context.runtime_flag with
             | WasReturn -> return loop_context
             | WasContinue ->
               eval_loop loop_stat { loop_context with runtime_flag = NoFlag }
             | _ -> eval_loop loop_stat loop_context)
          | _ -> error (str_of_error IncorrectType)
      in
      (match stat with
       | Block _ ->
         eval_loop
           stat
           { in_context with
             count_cycles = in_context.count_cycles + 1
           ; level = in_context.level + 1
           }
       | _ ->
         eval_loop stat { in_context with count_cycles = in_context.count_cycles + 1 })
    | For (stat_opt, expr_opt, after_list, body_stat) ->
      let* new_context =
        match stat_opt with
        | None -> return { in_context with level = in_context.level + 1 }
        | Some dec_stat ->
          eval_statement
            dec_stat
            { in_context with level = in_context.level + 1 }
            class_map
      in
      let* new_context = eval_inc_dec new_context in
      let remove_loop_vars (context' : context) =
        let var_seq = KeyMap.to_seq context'.var_map in
        let rec iter_classes (var_s : (key_setup * var) Seq.t) (context' : context) =
          match var_s () with
          | Seq.Nil -> return context'
          | Seq.Cons ((key_setup, x), xs) ->
            (match x.level = context'.level with
             | true ->
               iter_classes
                 xs
                 { context' with var_map = KeyMap.remove key_setup context'.var_map }
             | false -> iter_classes xs context')
        in
        let* context' = iter_classes var_seq context' in
        return context'
      in
      let rec eval_loop body_st af_list context =
        let* cond_context =
          match expr_opt with
          | None -> return { context with last_result = Bool' true }
          | Some expr_t -> eval_expr expr_t context class_map
        in
        let* cond_context = eval_inc_dec cond_context in
        match cond_context.last_result with
        | Bool' false ->
          remove_loop_vars
            { cond_context with
              count_cycles = cond_context.count_cycles - 1
            ; level = cond_context.level - 1
            }
        | Bool' true ->
          let rec interpret_list e_list as_context =
            match e_list with
            | [] ->
              let* as_context = eval_inc_dec as_context in
              return as_context
            | x :: xs ->
              if expr_in_statement x
              then
                let* next_context = eval_expr x as_context class_map in
                interpret_list xs next_context
              else error (str_of_error IncorrectType)
          in
          let* body_context =
            eval_statement
              body_st
              { cond_context with level = new_context.level + 1 }
              class_map
          in
          (match body_context.runtime_flag with
           | WasReturn -> return body_context
           | WasContinue ->
             let* after_context = interpret_list af_list body_context in
             eval_loop body_st af_list { after_context with runtime_flag = NoFlag }
           | WasBreak ->
             remove_loop_vars
               { context with
                 runtime_flag = NoFlag
               ; count_cycles = context.count_cycles - 1
               ; level = context.level - 1
               }
           | _ ->
             let* after_context = interpret_list af_list body_context in
             eval_loop body_st af_list after_context)
        | _ -> error (str_of_error IncorrectType)
      in
      eval_loop
        body_stat
        after_list
        { new_context with count_cycles = in_context.count_cycles + 1 }
    | Break ->
      if in_context.count_cycles <= 0
      then error (str_of_error BrokeDown)
      else return { in_context with runtime_flag = WasBreak }
    | Continue ->
      if in_context.count_cycles <= 0
      then error (str_of_error BrokeDown)
      else return { in_context with runtime_flag = WasContinue }
    | Return None when in_context.method_type = Void ->
      let* in_context = eval_inc_dec in_context in
      return { in_context with last_result = Void'; runtime_flag = WasReturn }
    | Return None -> error (str_of_error TypeMismatch)
    | Return (Some expr) ->
      let* ret_type = check_expr_type expr in_context class_map in
      if ret_type <> in_context.method_type
      then error (str_of_error TypeMismatch)
      else
        let* new_context = eval_expr expr in_context class_map in
        let* new_context = eval_inc_dec new_context in
        return { new_context with runtime_flag = WasReturn }
    | VariableDeclare (modifier, vars_type, var_list) ->
      let is_const : modifier option -> bool = function
        | Some Const -> true
        | _ -> false
      in
      let get_base_value = function
        | Float -> Float' 0.0
        | Int -> Int' 0
        | String -> String' ""
        | Class _ -> Null'
        | Bool -> Bool' false
        | Void -> Void'
      in
      let rec var_interpret var_list var_context =
        match var_list with
        | [] -> return var_context
        | (var_name, var_expr_opt) :: tail ->
          let* next_context =
            match var_expr_opt with
            | None ->
              let var =
                { var_key = var_name
                ; var_type = vars_type
                ; var_value = get_base_value vars_type
                ; is_const = is_const modifier
                ; assignment_count = 0
                ; level = var_context.level
                }
              in
              return (add_var var_context var)
            | Some var_expr ->
              let* var_expr_type = check_expr_type var_expr var_context class_map in
              (match var_expr_type with
               | _ when var_expr_type = vars_type ->
                 let* expr_context = eval_expr var_expr var_context class_map in
                 let* expr_context = eval_inc_dec expr_context in
                 let var =
                   { var_key = var_name
                   ; var_type = var_expr_type
                   ; var_value = expr_context.last_result
                   ; is_const = is_const modifier
                   ; assignment_count = 1
                   ; level = expr_context.level
                   }
                 in
                 return (add_var expr_context var)
               | _ -> error (str_of_error IncorrectType))
          in
          var_interpret tail next_context
      in
      var_interpret var_list in_context
    | Throw expr ->
      let* new_context = eval_expr expr in_context class_map in
      (match new_context.last_result with
       | Class' ex_cl ->
         (match ex_cl with
          | ObjectNull -> error (str_of_error BrokeDown)
          | ObjectRef (class_key, parent_key) ->
            (match parent_key with
             | Some "Exception" -> return { new_context with runtime_flag = WasThrown }
             | None ->
               (match class_key with
                | "Exception" -> return { new_context with runtime_flag = WasThrown }
                | _ -> error (str_of_error BrokeDown))
             | _ -> error (str_of_error BrokeDown)))
       | _ -> error (str_of_error BrokeDown))
    | Try (try_statement, catch_list, finally_stat_opt) ->
      let eval_try = function
        | Block _ ->
          let* t_context =
            eval_statement
              try_statement
              { in_context with level = in_context.level + 1 }
              class_map
          in
          return { t_context with level = t_context.level - 1 }
        | _ -> error (str_of_error BrokeDown)
      in
      let eval_finally finally_context =
        let eval_finally_statement stat class_map =
          let* f_context =
            eval_statement
              stat
              { finally_context with
                runtime_flag = NoFlag
              ; level = finally_context.level + 1
              }
              class_map
          in
          return f_context
        in
        match finally_stat_opt with
        | None -> return finally_context
        | Some (Block _ as finally_stat) when finally_context.runtime_flag = WasReturn ->
          let return_value = finally_context.last_result in
          let* f_context = eval_finally_statement finally_stat class_map in
          return
            { f_context with
              runtime_flag = WasReturn
            ; last_result = return_value
            ; level = f_context.level - 1
            }
        | Some (Block _ as finally_stat) ->
          let saved_flag = finally_context.runtime_flag in
          let* f_context = eval_finally_statement finally_stat class_map in
          return { f_context with runtime_flag = saved_flag; level = f_context.level - 1 }
        | _ -> error (str_of_error NotBrackets)
      in
      let* after_try_context = eval_try try_statement in
      (match after_try_context.runtime_flag = WasThrown with
       | true ->
         let check_catch_stat = function
           | Block _ -> return ()
           | _ -> error (str_of_error NotBrackets)
         in
         let eval_catch_stat catch_context catch_stat =
           let* catch_context =
             check_catch_stat catch_stat
             >> eval_statement
                  catch_stat
                  { catch_context with
                    runtime_flag = NoFlag
                  ; level = catch_context.level + 1
                  }
                  class_map
           in
           return { catch_context with level = catch_context.level - 1 }
         in
         let eval_catch catch_context = function
           | None, catch_stat -> eval_catch_stat catch_context catch_stat
           | Some (Class cl_name), catch_stat ->
             (match catch_context.last_result with
              | Class' (ObjectRef (thrown_name, _)) ->
                if thrown_name = cl_name || cl_name = "Exception"
                then eval_catch_stat catch_context catch_stat
                else return catch_context
              | _ -> error (str_of_error IncorrectType))
           | _ -> error (str_of_error IncorrectSyntax)
         in
         let rec eval_catch_list = function
           | [] -> return after_try_context
           | exc :: xs ->
             let* new_context = eval_catch after_try_context exc in
             (match new_context.runtime_flag = WasThrown with
              | false -> return new_context
              | true -> eval_catch_list xs)
         in
         let* after_catches_context = eval_catch_list catch_list in
         let* end_context = eval_finally after_catches_context in
         return end_context
       | false ->
         let* end_context = eval_finally after_try_context in
         return end_context)
    | Print print_expr ->
      let* new_context = eval_expr print_expr in_context class_map in
      let* new_context = eval_inc_dec new_context in
      let eval_printer = function
        | Float' value -> return (printf "%f\n" value)
        | Int' value -> return (printf "%d\n" value)
        | Bool' value -> return (printf "%b\n" value)
        | String' value -> return (printf "%s\n" value)
        | Class' value ->
          (match value with
           | ObjectNull -> error (str_of_error BrokeDown)
           | ObjectRef (class_key, _) -> return (printf "%s\n" class_key))
        | Void' -> error (str_of_error BrokeDown)
        | Null' -> error (str_of_error BrokeDown)
      in
      eval_printer new_context.last_result >> return new_context

  and eval_expr in_expr in_context class_map =
    let eval_helper e_expr context =
      let eval_l_r l_expr r_expr eval_f =
        let* context = eval_inc_dec context in
        let* l_context = eval_expr l_expr context class_map in
        let* r_context = eval_expr r_expr l_context class_map in
        try
          let ret_v = eval_f l_context.last_result r_context.last_result in
          return { r_context with last_result = ret_v }
        with
        | Invalid_argument m -> error m
      in
      match e_expr with
      | Binop (Plus, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' x, Int' y -> Int' (x + y)
          | Float' x, Float' y -> Float' (x +. y)
          | Float' x, Int' y -> Float' (x +. float_of_int y)
          | Int' x, Float' y -> Float' (float_of_int x +. y)
          | String' x, String' y -> String' (x ^ y)
          | Int' x, String' y -> String' (string_of_int x ^ y)
          | String' x, Int' y -> String' (x ^ string_of_int y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Binop (Minus, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' x, Int' y -> Int' (x - y)
          | Float' x, Float' y -> Float' (x -. y)
          | Float' x, Int' y -> Float' (x -. float_of_int y)
          | Int' x, Float' y -> Float' (float_of_int x -. y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Binop (Multiply, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' x, Int' y -> Int' (x * y)
          | Float' x, Float' y -> Float' (x *. y)
          | Float' x, Int' y -> Float' (x *. float_of_int y)
          | Int' x, Float' y -> Float' (float_of_int x *. y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Binop (Divide, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' _, Int' y when y = 0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' _, Float' y when y = 0.0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' _, Int' y when y = 0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Int' _, Float' y when y = 0.0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' x, Float' y -> Float' (x /. y)
          | Float' x, Int' y -> Float' (x /. float_of_int y)
          | Int' x, Float' y -> Float' (float_of_int x /. y)
          | Int' x, Int' y -> Int' (x / y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectArgumentType)))
      | Binop (Mod, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' _, Int' y when y = 0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' _, Float' y when y = 0.0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' _, Int' y when y = 0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Int' _, Float' y when y = 0.0 ->
            raise (Invalid_argument (str_of_error DivisionByZero))
          | Float' x, Float' y -> Float' (mod_float x y)
          | Float' x, Int' y -> Float' (mod_float x (float_of_int y))
          | Int' x, Float' y -> Float' (mod_float (float_of_int x) y)
          | Int' x, Int' y -> Int' (x mod y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectArgumentType)))
      | And (l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Bool' x, Bool' y -> Bool' (x && y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Or (l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Bool' x, Bool' y -> Bool' (x || y)
          | _, _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Not not_expr ->
        let* context = eval_inc_dec context in
        let* new_context = eval_expr not_expr context class_map in
        (match new_context.last_result with
         | Bool' x -> return { new_context with last_result = Bool' (not x) }
         | _ -> error (str_of_error IncorrectType))
      | Compare (Less, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Int' x, Int' y -> Bool' (x < y)
          | Float' x, Float' y -> Bool' (x < y)
          | Int' x, Float' y -> Bool' (float_of_int x < y)
          | Float' x, Int' y -> Bool' (x < float_of_int y)
          | _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Compare (More, l, r) -> eval_expr (Compare (Less, l, r)) context class_map
      | Compare (LessOrEqual, l, r) ->
        eval_expr (Not (Compare (More, l, r))) context class_map
      | Compare (MoreOrEqual, l, r) ->
        eval_expr (Not (Compare (Less, l, r))) context class_map
      | Compare (Equal, l, r) ->
        eval_l_r l r (fun lv rightv ->
          match lv, rightv with
          | Float' x, Float' y -> Bool' (x = y)
          | Float' x, Int' y -> Bool' (x = float_of_int y)
          | Int' x, Float' y -> Bool' (float_of_int x = y)
          | Int' x, Int' y -> Bool' (x = y)
          | Bool' x, Bool' y -> Bool' (x = y)
          | Void', Void' -> Bool' true
          | String' x, String' y -> Bool' (x = y)
          | Class' x, Class' y ->
            (match x, y with
             | ObjectNull, ObjectNull -> Bool' true
             | ObjectNull, _ | _, ObjectNull -> Bool' false
             | ObjectRef (key1, _), ObjectRef (key2, _) -> Bool' (key1 = key2))
          | _ -> raise (Invalid_argument (str_of_error IncorrectType)))
      | Compare (NotEqual, l, r) ->
        eval_expr (Not (Compare (Equal, l, r))) context class_map
      | Value value ->
        let* context = eval_inc_dec context in
        return { context with last_result = value }
      | Variable var_key ->
        let* context = eval_inc_dec context in
        (match KeyMap.find_opt var_key context.var_map with
         | Some var -> return { context with last_result = var.var_value }
         | None -> error (str_of_error (NoVariable var_key)))
      | Null -> return { context with last_result = Class' ObjectNull }
      | CallMethod (method_key, args) ->
        let* main_class = find_main_class class_map in
        let* method_s = find_method_monad main_class.method_map method_key in
        let* new_var_map, new_context =
          declare_var_map KeyMap.empty context args method_s.args class_map
        in
        let* new_context = eval_inc_dec new_context in
        let* res_context =
          eval_statement
            method_s.body
            { var_map = new_var_map
            ; method_type = method_s.method_type
            ; last_result = Void'
            ; runtime_flag = NoFlag
            ; count_cycles = 0
            ; level = 0
            ; increment = []
            ; decrement = []
            }
            class_map
        in
        (match res_context.runtime_flag with
         | WasThrown ->
           return
             { new_context with
               last_result = res_context.last_result
             ; runtime_flag = WasThrown
             }
         | _ ->
           return
             { new_context with
               last_result =
                 (if method_s.method_type = Void then Void' else res_context.last_result)
             })
      | Assign (Variable var_key, val_expr) ->
        let* context = eval_inc_dec context in
        let* assign_context = eval_expr val_expr context class_map in
        (match KeyMap.find_opt var_key assign_context.var_map with
         | None -> error (str_of_error (NoVariable var_key))
         | Some old_var ->
           let* _ = check_assign_var old_var in
           let var =
             { old_var with
               var_value = assign_context.last_result
             ; assignment_count = old_var.assignment_count + 1
             }
           in
           return (replace_var assign_context var_key var))
      | IncrementPost (Variable var_key) ->
        let* context =
          eval_expr
            (Assign (Variable var_key, Binop (Plus, Value (Int' 0), Variable var_key)))
            context
            class_map
        in
        return { context with increment = var_key :: context.increment }
      | DecrementPost (Variable var_key) ->
        let* context =
          eval_expr
            (Assign (Variable var_key, Binop (Minus, Value (Int' 0), Variable var_key)))
            context
            class_map
        in
        return { context with decrement = var_key :: context.decrement }
      | ClassCreate (class_name, _) ->
        let get_elem_map class_map key_setup =
          match KeyMap.find_opt key_setup class_map with
          | None -> error (str_of_error BrokeDown)
          | Some elem -> return elem
        in
        let* curr_class = get_elem_map class_map class_name in
        let class_key = curr_class.class_key in
        let parent_key = curr_class.parent_key in
        return { context with last_result = Class' (ObjectRef (class_key, parent_key)) }
      | _ -> error (str_of_error IncorrectType)
    in
    eval_helper in_expr in_context

  and eval_inc_dec context =
    let rec eval_inc_dec_crement crement_context inc_v remove_f = function
      | [] -> return crement_context
      | x :: xs ->
        (match KeyMap.find_opt x crement_context.var_map with
         | None -> error (str_of_error (NoVariable x))
         | Some old_var ->
           let* _ = check_assign_var old_var in
           let change_value =
             match old_var.var_value with
             | Int' v -> return (Int' (v + inc_v))
             | _ -> error (str_of_error IncorrectType)
           in
           let* value = change_value in
           let var =
             { old_var with
               var_value = value
             ; assignment_count = old_var.assignment_count + 1
             }
           in
           let* new_context = return (replace_var crement_context x var) in
           eval_inc_dec_crement (remove_f new_context x) inc_v remove_f xs)
    in
    let* inc_context =
      eval_inc_dec_crement context 1 remove_key_increment context.increment
    in
    let* dec_context =
      eval_inc_dec_crement inc_context (-1) remove_key_decrement inc_context.decrement
    in
    return dec_context

  and declare_var_map varm context args meth_args class_map =
    let update_var var_context arg = function
      | var_type, var_key ->
        let* new_context = eval_expr arg var_context class_map in
        let var =
          { var_type
          ; var_key
          ; is_const = false
          ; assignment_count = 1
          ; var_value = new_context.last_result
          ; level = 0
          }
        in
        let add_context = add_var new_context var in
        return (add_context.var_map, add_context)
    in
    let rec iter_vars (var_map, var_context) var_args var_meth_args =
      match var_args, var_meth_args with
      | [], [] -> return (var_map, var_context)
      | x :: xs, y :: ys ->
        let* new_vm, new_context = update_var var_context x y in
        iter_vars (new_vm, new_context) xs ys
      | _, _ -> error (str_of_error IncorrectType)
    in
    iter_vars (varm, context) args meth_args

  and check_assign_var : var -> unit M.t =
   fun var ->
    match var.assignment_count with
    | 0 -> return ()
    | _ when not var.is_const -> return ()
    | _ -> error (str_of_error IncorrectSyntax)
 ;;

  let start_interpret : class_setup KeyMap.t -> context M.t =
   fun class_map ->
    let* main_class = find_main_class class_map in
    let* context = init_contex KeyMap.empty in
    let main = KeyMap.find "Main" main_class.method_map in
    let* final_context = eval_statement main.body context class_map in
    match final_context.runtime_flag = WasThrown with
    | false -> return final_context
    | true -> error (str_of_error BrokeDown)
 ;;

  let rec monadic_list_ite action list ret =
    match list with
    | [] -> return ret
    | x :: xs -> action x >> monadic_list_ite action xs ret
  ;;

  let system_excep_init class_map =
    let field_map = KeyMap.empty in
    let method_map = KeyMap.empty in
    let body = Block [ Return (Some (Variable "Message")) ] in
    let method_key : method_setup =
      { method_type = String; method_key = "ToString"; args = []; body }
    in
    let field_key : field_setup =
      { field_type = String; field_key = "Message"; is_const = false; sub_tree = None }
    in
    let declare_class =
      ClassDec
        ( [ Public ]
        , "Exception"
        , None
        , [ [ Public ], VariableField (String, [ "Message", None ])
          ; ( [ Public ]
            , Method (String, "ToString", [], Block [ Return (Some (Variable "Message")) ])
            )
          ] )
    in
    let field_map = KeyMap.add "Message" field_key field_map in
    let method_map = KeyMap.add "ToString" method_key method_map in
    let class_map =
      KeyMap.add
        "Exception"
        { class_key = "Exception"
        ; field_map
        ; method_map
        ; parent_key = None
        ; declare_class
        }
        class_map
    in
    return class_map
  ;;

  let classes_add class_list_ast class_map =
    let class_in_map cl_map class_add =
      match class_add with
      | ClassDec (_, class_key, parent, fields) ->
        let class_elem_add field_elem field_map method_map =
          match field_elem with
          | mod_list, VariableField (field_type, arg_list) ->
            let rec helper_add_var list field_map method_map =
              match list with
              | [] -> return (field_map, method_map)
              | (field_key, sub_tree) :: ps ->
                let is_const = List.mem Const mod_list in
                let* field_m, method_m =
                  match KeyMap.find_opt field_key field_map with
                  | None ->
                    let field_map =
                      KeyMap.add
                        field_key
                        { field_type; field_key; is_const; sub_tree }
                        field_map
                    in
                    return (field_map, method_map)
                  | _ -> error (str_of_error (ThisNameAlreadyExists field_key))
                in
                helper_add_var ps field_m method_m
            in
            helper_add_var arg_list field_map method_map
          | _, Method (method_type, method_key, args, body) ->
            (match KeyMap.find_opt method_key method_map with
             | None ->
               let method_map =
                 KeyMap.add method_key { method_type; method_key; args; body } method_map
               in
               return (field_map, method_map)
             | _ -> error (str_of_error (ThisNameAlreadyExists method_key)))
        in
        let rec item_fields fields field_map method_map =
          match fields with
          | [] -> return (field_map, method_map)
          | x :: xs ->
            let* field_m, method_m = class_elem_add x field_map method_map in
            item_fields xs field_m method_m
        in
        let* field_map, method_map = item_fields fields KeyMap.empty KeyMap.empty in
        let parent_key = parent in
        (match KeyMap.find_opt class_key cl_map with
         | None ->
           let class_setup =
             { class_key; field_map; method_map; parent_key; declare_class = class_add }
           in
           let cl_map = KeyMap.add class_key class_setup cl_map in
           return cl_map
         | _ -> error (str_of_error (ThisNameAlreadyExists class_key)))
    in
    let rec iter_classes class_list_ast class_map =
      match class_list_ast with
      | [] -> return class_map
      | x :: xs ->
        let* class_m = class_in_map class_map x in
        iter_classes xs class_m
    in
    let* class_m = iter_classes class_list_ast class_map in
    return class_m
  ;;

  let interpret_cl class_list_ast class_map =
    match class_list_ast with
    | [] -> error (str_of_error IncorrectSyntax)
    | _ ->
      let* class_map_with_ex = system_excep_init class_map in
      classes_add class_list_ast class_map_with_ex
  ;;
end
