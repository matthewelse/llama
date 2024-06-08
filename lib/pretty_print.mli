open! Core

val pp_type : Formatter.t -> Type.t -> unit
val pp_polytype : Formatter.t -> Type.Poly.t -> unit

module For_testing : sig
  val pp_tv' : Formatter.t -> int -> unit
end
