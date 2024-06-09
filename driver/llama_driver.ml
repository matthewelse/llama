open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Run the Llama compiler"
    [%map_open.Command
      let source_file = anon ("SRC" %: File_path.arg_type)
      and dump_ast = flag "dump-ast" no_arg ~doc:"Dump the AST after parsing" in
      fun () ->
        let _ast =
          In_channel.with_file (File_path.to_string source_file) ~f:(fun in_channel ->
            let lexbuf = Lexing.from_channel in_channel in
            let ast = Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf in
            if dump_ast
            then (
              Llama_utils.Pretty_print.pp_ast Format.std_formatter ast;
              Format.pp_print_newline Format.std_formatter ());
            ast)
        in
        Deferred.Or_error.return ()]
;;
