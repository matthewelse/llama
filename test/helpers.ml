open! Core
open! Import
module Env = Llama_typing.Env

let parse_with_error_reporting code ~pp_ast =
  let lexbuf = Lexing.from_string code in
  Lexing.set_filename lexbuf "<example>";
  match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
  | exception Llama_frontend.Parser.Error n ->
    let message =
      try Llama_frontend.Errors.message n |> String.rstrip with
      | _ -> [%string "Unknown error %{n#Int}"]
    in
    let error_output =
      Diagnostics.create
        ~code
        ~message:"Syntax error"
        ~error_code:[%string "E%{n#Int}"]
        ~error_offset:(Lexing.lexeme_start_p lexbuf)
        ~labels:
          { primary =
              { span = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf; message }
          ; secondary = []
          }
        Error
    in
    Diagnostics.render error_output Format.err_formatter;
    Error ()
  | ast ->
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    Ok ast
;;

let test_fragment ?(pp_ast = false) code =
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  (ignore : (unit, unit) result -> unit)
  @@
  let%bind.Result ast = parse_with_error_reporting code ~pp_ast in
  let result = Llama_typing.Infer.type_ast ast in
  match result with
  | Ok env ->
    print_s [%message (env : Env.t)];
    Ok ()
  | Error { primary_location; message } ->
    let error_output =
      Diagnostics.create
        ~code
        ~message:"Type Error"
        ~error_code:[%string "EXXXX"]
        ~error_offset:(fst primary_location)
        ~labels:{ primary = { span = primary_location; message }; secondary = [] }
        Error
    in
    Diagnostics.render error_output Format.err_formatter;
    Ok ()
;;
