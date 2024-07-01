open! Core
open! Import

type t =
  { message : string
  ; primary_location : Span.t
  }
[@@deriving sexp_of]

val of_string : loc:Span.t -> string -> t
val error_string : loc:Span.t -> string -> (_, t) result
val error_s : loc:Span.t -> Sexp.t -> (_, t) result
val ok_exn : ('a, t) result -> 'a
