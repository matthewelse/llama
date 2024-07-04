open! Core
open! Import
module Env = Llama_typing.Env
module Terminal = Asai.Tty.Make (Llama_common.Reporter.Message)

let parse_with_error_reporting code ~pp_ast =
  let lexbuf = Lexing.from_string code in
  Lexing.set_filename lexbuf "<example>";
  match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
  | exception Llama_frontend.Parser.Error n ->
    let message =
      try Llama_frontend.Errors.message n |> String.rstrip with
      | _ -> [%string "Unknown error %{n#Int}"]
    in
    let source : Asai.Range.source = `String { title = None; content = code } in
    Llama_common.Reporter.fatal
      ~loc:(Asai.Range.of_lexbuf ~source lexbuf)
      (Parse_error n)
      message
  | ast ->
    if pp_ast then Pretty_print.pp_ast Format.std_formatter ast;
    ast
;;

let test_fragment ?(pp_ast = false) ?(output = `Env) code =
  let source : Asai.Range.string_source = { title = None; content = code } in
  Type.Var.For_testing.reset_counter ();
  Type.Id.For_testing.reset_counter ();
  Llama_common.Reporter.run
    ~emit:(Terminal.display ~use_color:false ~override_source:(`String source))
    ~fatal:(fun d ->
      Terminal.display ~use_color:false ~override_source:(`String source) d)
    (fun () ->
      let ast = parse_with_error_reporting code ~pp_ast in
      let result = Llama_typing.Infer.type_ast ast in
      match result with
      | Ok env ->
        (match output with
         | `Env -> print_s [%message (env : Env.t)]
         | `Values ->
           env.values
           |> Map.iteri ~f:(fun ~key ~data ->
             Format.printf
               "val %s : %a\n"
               (Ident.to_string key)
               Pretty_print.pp_polytype
               data))
      | Error { primary_location; message } ->
        let source : Asai.Range.source = `String { title = None; content = code } in
        Llama_common.Reporter.fatal
          ~loc:(Asai.Range.of_lex_range ~source primary_location)
          Type_error
          message)
;;
