open! Core
open! Import

type t =
  { values : Type.Poly.t Ident.Map.t
  ; type_declarations : Type.Constructor.t Type_name.Map.t
  ; constructors : Type_name.t Constructor.Map.t
  ; fields : Type_name.t Field_name.Map.t
  }
[@@deriving sexp_of]

let empty =
  { values = Ident.Map.empty
  ; type_declarations = Type_name.Map.empty
  ; constructors = Constructor.Map.empty
  ; fields = Field_name.Map.empty
  }
;;

let field t name =
  Map.find t.fields name
  |> Or_error.of_option
       ~error:(Error.of_string [%string "Unbound field [%{name#Field_name}]"])
;;

let constructor t name =
  Map.find t.constructors name
  |> Or_error.of_option
       ~error:(Error.of_string [%string "Unbound constructor %{name#Constructor}"])
;;

let type_declaration t name =
  Map.find t.type_declarations name
  |> Or_error.of_option
       ~error:(Error.of_string [%string "Unbound type %{name#Type_name}"])
;;

let value t name =
  Map.find t.values name
  |> Or_error.of_option
       ~error:(Error.of_string [%string "Unbound variable [%{name#Ident}]"])
;;

let with_fields t fields ~type_name =
  let%bind.Or_error fields =
    List.fold_result fields ~init:t.fields ~f:(fun fields (name, _) ->
      match Map.add fields ~key:name ~data:type_name with
      | `Ok fields -> Ok fields
      | `Duplicate ->
        Error (Error.of_string [%string "Duplicate field [%{name#Field_name}]"]))
  in
  Ok { t with fields }
;;

let with_constructors t constructors ~type_name =
  { t with
    constructors =
      List.fold constructors ~init:t.constructors ~f:(fun fields (name, _) ->
        Map.add_exn fields ~key:name ~data:type_name)
  }
;;

let with_type_declaration t name ty =
  { t with type_declarations = Map.add_exn t.type_declarations ~key:name ~data:ty }
;;

let with_vars t vars =
  let values =
    List.fold vars ~init:t.values ~f:(fun env (name, var) ->
      Map.set env ~key:name ~data:{ ty = Var var; quantifiers = Type.Var.Set.empty })
  in
  { t with values }
;;

let with_var t name ty = { t with values = Map.set t.values ~key:name ~data:ty }
