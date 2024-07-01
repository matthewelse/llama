open! Core
open! Import

type t =
  { message : string
  ; primary_location : Span.t
  }
[@@deriving sexp_of]

let of_string ~loc message = { message; primary_location = loc }
let error_string ~loc message = Error { message; primary_location = loc }
let error_s ~loc s = Error { message = Sexp.to_string_hum s; primary_location = loc }

let ok_exn res =
  match res with
  | Ok x -> x
  | Error { message; primary_location } ->
    raise_s [%message message (primary_location : Span.t)]
;;
