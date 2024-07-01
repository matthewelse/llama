open! Core
open! Async
module Terminal = Asai.Tty.Make (Llama_common.Reporter.Message)

let type_check_with_error_reporting ?env ?source ast ~dump_type_env =
  let env = Option.value_or_thunk env ~default:Llama_typing.Env.empty in
  match Llama_typing.Infer.type_ast ast ~env with
  | Ok env ->
    if dump_type_env
    then (
      Core.print_s [%message (env : Llama_typing.Env.t)];
      Core.printf "%!");
    env
  | Error { message; primary_location } ->
    Llama_common.Reporter.fatal
      ~loc:(Asai.Range.of_lex_range ?source primary_location)
      Type_error
      message
;;

let parse_with_error_reporting ?source lexbuf ~dump_ast =
  match Llama_frontend.Parser.program Llama_frontend.Lexer.read lexbuf with
  | exception Llama_frontend.Parser.Error n ->
    Llama_common.Reporter.fatal
      ~loc:(Asai.Range.of_lexbuf ?source lexbuf)
      (Parse_error n)
      (Llama_frontend.Errors.message n |> String.strip)
  | ast ->
    if dump_ast
    then (
      Llama_utils.Pretty_print.pp_ast Format.std_formatter ast;
      Format.pp_print_newline Format.std_formatter ());
    ast
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
        let () =
          In_channel.with_file (File_path.to_string source_file) ~f:(fun in_channel ->
            let lexbuf = Lexing.from_channel in_channel in
            Lexing.set_filename lexbuf (File_path.to_string source_file);
            Llama_common.Reporter.run
              ~emit:Terminal.display
              ~fatal:(fun d -> Terminal.display d)
              (fun () ->
                let ast = parse_with_error_reporting lexbuf ~dump_ast in
                let (_ : Llama_typing.Env.t) =
                  type_check_with_error_reporting ast ~dump_type_env
                in
                ()))
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
        let env = ref (Llama_typing.Env.empty ()) in
        let%bind () =
          Deferred.repeat_until_finished () (fun () ->
            let code = LNoise.linenoise "llama> " in
            match code with
            | Some ".exit" -> return (`Finished ())
            | Some code ->
              LNoise.history_add code |> (ignore : _ result -> unit);
              let source : Asai.Range.source = `String { title = None; content = code } in
              let lexbuf = Lexing.from_string code in
              Lexing.set_filename lexbuf "<stdin>";
              return
              @@ Llama_common.Reporter.run
                   ~emit:Terminal.display
                   ~fatal:(fun d ->
                     Terminal.display d;
                     `Repeat ())
                   (fun () ->
                     let env' =
                       let ast = parse_with_error_reporting lexbuf ~source ~dump_ast in
                       type_check_with_error_reporting
                         ast
                         ~source
                         ~dump_type_env
                         ~env:!env
                     in
                     env := env';
                     `Repeat ())
            | None -> (* This usually means sigint *) return (`Finished ()))
        in
        Deferred.Or_error.return ()]
;;

let command = Command.group ~summary:"Llama driver" [ "compile", compile; "repl", repl ]
