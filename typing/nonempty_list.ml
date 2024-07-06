open! Core
open! Import

type 'a t = ( :: ) of 'a * 'a list

let hd (hd :: _) = hd
let sexp_of_t sexp_of_a (hd :: tl) = [%sexp_of: a list] (hd :: tl)
