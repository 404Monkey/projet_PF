type token =
  | Add    (* Binary operator for addition (+) *)
  | Subtract    (* Binary operator for subtraction (-) *)
  | Multiply    (* Binary operator for multiplication [*] *)
  | Divide    (* Binary operator for division (/) *)
  | Minus    (* Unary operator for opposite (~) *)
  | Variable of char    (* Variable names are lowercase unaccented letters *)
  | Number of int    (* Only nonnegative integer numbers *)
  | End    (* End of expression (;) *)
val input_to_token_list : unit -> token list
val string_to_token_list : string -> token list
exception Lexical_Error of string
