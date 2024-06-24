open! Core

module Level = struct
  type t =
    | Error
    | Warning
    | Info
  [@@deriving sexp_of]
end

module Span = struct
  type t = Source_code_position.t * Source_code_position.t [@@deriving sexp_of]

  let col_range ((start, end_) : t) =
    start.pos_cnum - start.pos_bol, end_.pos_cnum - start.pos_bol
  ;;
end

module Label = struct
  type t =
    { span : Span.t
    ; message : string
    }
  [@@deriving sexp_of]

  let range { span = start, end_; _ } = start.pos_bol, end_.pos_cnum
end

module Labels = struct
  type t =
    { primary : Label.t
    ; secondary : Label.t list
    }
  [@@deriving sexp_of]

  let single_line_labels t =
    let primary_start, primary_end = t.primary.span in
    let primary =
      if primary_start.pos_lnum = primary_end.pos_lnum
      then [ primary_start.pos_lnum, (`Primary, t.primary) ]
      else []
    in
    Int.Map.of_alist_multi
    @@ primary
    @ List.concat_map t.secondary ~f:(fun label ->
      let start, end_ = label.span in
      if start.pos_lnum = end_.pos_lnum
      then [ start.pos_lnum, (`Secondary, label) ]
      else [])
  ;;

  let range t =
    let start, end_ = Label.range t.primary in
    let start, end_ =
      List.fold t.secondary ~init:(start, end_) ~f:(fun (start, end_) label ->
        let label_start, label_end = Label.range label in
        Int.min start label_start, Int.max end_ label_end)
    in
    start, `First_newline_after end_
  ;;

  let relevant_code t code =
    let start_pos, `First_newline_after end_pos = range t in
    let end_pos =
      String.index_from code end_pos '\n' |> Option.value ~default:(String.length code)
    in
    String.sub code ~pos:start_pos ~len:(end_pos - start_pos)
  ;;

  let start_line t =
    List.fold t.secondary ~init:(fst t.primary.span).pos_lnum ~f:(fun acc secondary ->
      Int.min acc (fst secondary.span).pos_lnum)
  ;;
end

type t =
  { level : Level.t
  ; error_offset : Source_code_position.t
  ; message : string option
  ; code : string
  ; error_code : string option
  ; labels : Labels.t
  }
[@@deriving sexp_of]

let create ?error_code ?message ~code ~labels ~error_offset level =
  { error_code; level; code; labels; message; error_offset }
;;

let relevant_code t = Labels.relevant_code t.labels t.code

let should_colour =
  lazy
    (let term = Sys.getenv "TERM" in
     Option.is_none (Sys.getenv "NO_COLOR")
     && Option.is_some term
     && (not ([%compare.equal: string option] term (Some "dumb")))
     && not (Expect_test_helpers_core.am_running_expect_test ()))
;;

let render t fmt =
  (*
     {v
     error[$error_code]: $message
        -- $pos_fname:$pos_lnum:$pos_col
        |
     $k | ***** $line_k
        | *   *    ---- single line error message
     $n | *   * $line_n
        | * X *       ^^^^ primary error message
        | *   *  ---- other errors on the same line
        | *   *
        | *****
        |
     ^  ^ always shown, spans the entire block
     |
     line numbers

     $X: vertical sections for spans
     v}
  *)
  if force should_colour then Ocolor_format.prettify_formatter fmt;
  let start_line = Labels.start_line t.labels in
  let relevant_code = Labels.relevant_code t.labels t.code |> String.split_lines in
  let message = Option.value ~default:"" t.message in
  let error_code =
    match t.error_code with
    | None -> ""
    | Some error_code -> "[" ^ error_code ^ "]"
  in
  let single_line_labels = Labels.single_line_labels t.labels in
  let max_line_number_length =
    String.length (Int.to_string (String.count t.code ~f:(Char.equal '\n')))
  in
  let space_padding = String.make (max_line_number_length + 1) ' ' in
  Format.fprintf fmt {|@{<red>error%s:@} %s|} error_code message;
  Format.pp_print_newline fmt ();
  Format.pp_print_string
    fmt
    [%string
      "%{space_padding}╭─[%{t.error_offset.pos_fname}:%{t.error_offset.pos_lnum#Int}:%{t.error_offset.pos_cnum \
       - t.error_offset.pos_bol#Int}]"];
  Format.pp_print_newline fmt ();
  List.iteri relevant_code ~f:(fun i line ->
    let line_number = start_line + i in
    let labels = Map.find_multi single_line_labels line_number in
    Format.pp_print_string fmt [%string "%{line_number#Int} │ %{line}"];
    Format.pp_print_newline fmt ();
    List.iter labels ~f:(fun (primary_or_secondary, label) ->
      let annotation_char =
        match primary_or_secondary with
        | `Primary -> '^'
        | `Secondary -> '-'
      in
      let annotation =
        let start_col, end_col = Span.col_range label.span in
        String.make start_col ' ' ^ String.make (end_col - start_col) annotation_char
      in
      let spaces = space_padding in
      Format.pp_print_string fmt [%string "%{spaces}┆ %{annotation} %{label.message}"];
      Format.pp_print_newline fmt ()));
  let horizontal_part =
    List.init (max_line_number_length + 1) ~f:(Fn.const "─") |> String.concat
  in
  Format.pp_print_string fmt [%string "%{horizontal_part}╯"];
  Format.pp_print_newline fmt ()
;;

let unindent code =
  let min_indent =
    List.filter_map (String.split_lines code) ~f:(fun line ->
      match String.findi line ~f:(fun _ c -> not (Char.is_whitespace c)) with
      | None -> None
      | Some (i, _) -> Some (i - 1))
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  String.split_lines code
  |> List.map ~f:(fun line ->
    if String.length line < min_indent then "" else String.subo line ~pos:min_indent)
  |> String.concat ~sep:"\n"
;;

let%expect_test "example" =
  let example =
    {|module FizzBuzz where

fizz : Nat -> String
fizz num =
    case (mod num 5) (mod num 3) of
        0 0 => "FizzBuzz"
        0 _ => "Fizz"
        _ 0 => "Buzz"
        _ _ => num|}
  in
  (* [position_of code ~substr ~prefix] returns a [Source_code_position.t] corresponding to the
     position of [substr] within [code]. *)
  let position_of code ~substr ~prefix ~fname : Span.t =
    let pos_whole = String.substr_index_exn code ~pattern:(prefix ^ substr) in
    let pos_cnum = pos_whole + String.length prefix in
    let pos_bol = String.rindex_from_exn code pos_cnum '\n' + 1 in
    let pos_lnum =
      String.counti code ~f:(fun i c -> Char.equal c '\n' && i < pos_cnum) + 1
    in
    assert (not (String.mem substr '\n'));
    assert (pos_bol = 0 || Char.equal code.[pos_bol - 1] '\n');
    assert (Char.equal code.[pos_cnum] substr.[0]);
    ( { pos_cnum; pos_lnum; pos_bol; pos_fname = fname }
    , { pos_cnum = pos_cnum + String.length substr; pos_lnum; pos_bol; pos_fname = fname }
    )
  in
  [%expect {||}];
  let t =
    create
      ~error_code:"E0308"
      ~labels:
        { primary =
            { span =
                position_of example ~substr:"num" ~prefix:"_ _ => " ~fname:"FizzBuzz.fun"
            ; message = "expected `String`, found `Nat`"
            }
        ; secondary =
            [ { span =
                  position_of
                    example
                    ~substr:"\"FizzBuzz\""
                    ~prefix:"0 0 => "
                    ~fname:"FizzBuzz.fun"
              ; message = "this is found to be of type `String`"
              }
            ; { span =
                  position_of
                    example
                    ~substr:"\"Fizz\""
                    ~prefix:"0 _ => "
                    ~fname:"FizzBuzz.fun"
              ; message = "this is found to be of type `String`"
              }
            ; { span =
                  position_of
                    example
                    ~substr:"\"Buzz\""
                    ~prefix:"_ 0 => "
                    ~fname:"FizzBuzz.fun"
              ; message = "this is found to be of type `String`"
              }
            ; { span =
                  position_of
                    example
                    ~substr:"String"
                    ~prefix:"Nat -> "
                    ~fname:"FizzBuzz.fun"
              ; message = "expected type `String`, found here"
              }
            ]
        }
      ~code:example
      ~message:"`case` clauses have incompatible types"
      ~error_offset:
        (position_of example ~substr:"num" ~prefix:"_ _ => " ~fname:"FizzBuzz.fun" |> fst)
      Error
  in
  Expect_test_helpers_core.require_equal
    [%here]
    (module String)
    (relevant_code t)
    "fizz : Nat -> String\n\
     fizz num =\n\
    \    case (mod num 5) (mod num 3) of\n\
    \        0 0 => \"FizzBuzz\"\n\
    \        0 _ => \"Fizz\"\n\
    \        _ 0 => \"Buzz\"\n\
    \        _ _ => num";
  [%expect {||}];
  render t Format.std_formatter;
  [%expect
    {|
    error[E0308]: `case` clauses have incompatible types
      ╭─[FizzBuzz.fun:9:15]
    3 │ fizz : Nat -> String
      ┆               ------ expected type `String`, found here
    4 │ fizz num =
    5 │     case (mod num 5) (mod num 3) of
    6 │         0 0 => "FizzBuzz"
      ┆                ---------- this is found to be of type `String`
    7 │         0 _ => "Fizz"
      ┆                ------ this is found to be of type `String`
    8 │         _ 0 => "Buzz"
      ┆                ------ this is found to be of type `String`
    9 │         _ _ => num
      ┆                ^^^ expected `String`, found `Nat`
    ──╯
    |}]
;;
