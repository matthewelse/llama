open! Core
open! Import

(* FIXME: The whole name of this module, and the name of this function don't really make sense --
   really what we're doing is generating type constraints, and the typed ast at the same time. *)

val infer
  :  Expression.t
  -> env:Env.t
  -> Solver.Constraint.t list * Typed_ast.Expression.t * Type.t

module For_testing : sig
  val check_pattern
    :  Pattern.t
    -> Type.t
    -> constraints:Solver.Constraint.t Queue.t
    -> env:Env.t
    -> Typed_ast.Pattern.t * Env.t
end
