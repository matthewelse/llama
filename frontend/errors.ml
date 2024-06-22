(* This file was auto-generated based on "parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)
open! Core

let message s =
  match s with
  | 112 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 110 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 109 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 108 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 107 -> "Expected `:`.\n"
  | 106 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 81 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 79 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 71 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 70 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 64 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 63 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 62 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 57 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 54 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 53 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 52 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 50 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 49 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 47 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 46 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 45 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 42 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 41 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 40 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 38 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 37 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 33 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 32 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 31 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 26 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 25 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 17 -> "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
  | 19 -> "Expected a type.\n"
  | 14 -> "Expected a type.\n"
  | 12 -> "Expected a type.\n"
  | 9 -> "Expected a type.\n"
  | 8 -> "Expected `:`.\n"
  | 7 -> "Expected a record field name.\n"
  | 4 -> "Expected a record or a variant type declaration.\n"
  | 3 -> "Expected `=`.\n"
  | 2 -> "Expected a type name.\n"
  | 1 -> "Expected type variables or a type name.\n"
  | 0 -> "Expected `type`, `let` or `intrinsic`.\n"
  | n ->
    failwith
      [%string "Parser_messages.message: no message defined for the given code %{n#Int}"]
;;
