open! Core
open! Import

let type_ast ast ~env =
  let open Result.Let_syntax in
  let%bind env, constraints = Constraints.type_ast ast ~env in
  Solver.solve constraints ~env
;;
