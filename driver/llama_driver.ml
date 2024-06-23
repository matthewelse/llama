open! Core
open! Async

let parse_with_error_reporting lexbuf ~get_code ~dump_ast =
  match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
  | exception Llama_frontend.Parser.Error n ->
    let code = get_code () in
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
    in
    Diagnostics.render error_output Format.err_formatter;
    Error 1
  | ast ->
    if dump_ast
    then (
      Llama_utils.Pretty_print.pp_ast Format.std_formatter ast;
      Format.pp_print_newline Format.std_formatter ());
    Ok ast
;;

let compile =
  Command.async_or_error
    ~summary:"Run the Llama compiler"
    [%map_open.Command
      let source_file = anon ("SRC" %: File_path.arg_type)
      and dump_ast = flag "dump-ast" no_arg ~doc:"Dump the AST after parsing"
      and dump_type_env =
        flag "dump-types" no_arg ~doc:"Dump the type environment after type-checking."
      in
      fun () ->
        let%bind () =
          In_channel.with_file (File_path.to_string source_file) ~f:(fun in_channel ->
            let lexbuf = Lexing.from_channel in_channel in
            Lexing.set_filename lexbuf (File_path.to_string source_file);
            match
              parse_with_error_reporting
                lexbuf
                ~get_code:(fun () ->
                  In_channel.seek in_channel 0L;
                  In_channel.input_all in_channel)
                ~dump_ast
            with
            | Ok ast ->
              (match Llama_typing.Infer.type_ast ast ~env:Llama_typing.Env.empty with
               | Ok env ->
                 if dump_type_env
                 then (
                   print_s [%message (env : Llama_typing.Env.t)];
                   printf "%!")
               | Error error ->
                 Core.eprint_s [%message "Error while type checking" (error : Error.t)]);
              return ()
            | Error n -> exit n)
        in
        Deferred.Or_error.return ()]
;;

let repl =
  Command.async_or_error
    ~summary:"Llama repl"
    [%map_open.Command
      let dump_ast = flag "dump-ast" no_arg ~doc:"Dump the AST after parsing"
      and dump_type_env =
        flag "dump-types" no_arg ~doc:"Dump the type environment after type-checking."
      in
      fun () ->
        let env = ref Llama_typing.Env.empty in
        while
          let code = LNoise.linenoise "llama> " in
          match code with
          | Some ".exit" -> false
          | Some code ->
            LNoise.history_add code |> (ignore : _ result -> unit);
            let lexbuf = Lexing.from_string code in
            Lexing.set_filename lexbuf "<stdin>";
            (match
               parse_with_error_reporting lexbuf ~get_code:(fun () -> code) ~dump_ast
             with
             | Ok ast ->
               (match Llama_typing.Infer.type_ast ~env:!env ast with
                | Ok env' ->
                  env := env';
                  if dump_type_env
                  then Core.print_s [%message (!env : Llama_typing.Env.t)]
                | Error error ->
                  Core.eprint_s [%message "Error while type checking" (error : Error.t)])
             | Error _ -> ());
            true
          | None -> (* This usually means sigint *) false
        do
          ()
        done;
        Deferred.Or_error.return ()]
;;

let command = Command.group ~summary:"Llama driver" [ "compile", compile; "repl", repl ]
