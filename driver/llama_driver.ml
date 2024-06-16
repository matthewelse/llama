open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Run the Llama compiler"
    [%map_open.Command
      let source_file = anon ("SRC" %: File_path.arg_type)
      and dump_ast = flag "dump-ast" no_arg ~doc:"Dump the AST after parsing" in
      fun () ->
        In_channel.with_file (File_path.to_string source_file) ~f:(fun in_channel ->
          let lexbuf = Lexing.from_channel in_channel in
          Lexing.set_filename lexbuf (File_path.to_string source_file);
          match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
          | exception Llama_frontend.Parser.Error n ->
            In_channel.seek in_channel 0L;
            let code = In_channel.input_all in_channel in
            let error_output =
              Diagnostics.create
                ~code
                ~message:"Syntax error"
                ~error_code:[%string "E%{n#Int}"]
                ~error_offset:(Lexing.lexeme_start_p lexbuf)
                ~labels:
                  { primary =
                      { span = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf
                      ; message = Llama_frontend.Errors.message n |> String.rstrip
                      }
                  ; secondary = []
                  }
                Error
              |> Diagnostics.render
            in
            eprintf "%s\n" error_output
          | ast ->
            if dump_ast
            then (
              Llama_utils.Pretty_print.pp_ast Format.std_formatter ast;
              Format.pp_print_newline Format.std_formatter ()));
        Deferred.Or_error.return ()]
;;
