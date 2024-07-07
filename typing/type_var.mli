open! Core
open! Import

type t [@@immediate]

include Unique_id.Id with type t := t
