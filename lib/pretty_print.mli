open! Core
open! Import

val pp_type : Formatter.t -> Type.t -> unit
val pp_polytype : Formatter.t -> Type.Poly.t -> unit
val pp_expr : Formatter.t -> Expression.t -> unit
val pp_ast : Formatter.t -> Ast.t -> unit

module For_testing : sig
  val pp_tv' : Formatter.t -> int -> unit
end
