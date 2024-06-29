open! Core
open! Import

type t =
  { values : Type.Poly.t Ident.Map.t
  ; type_declarations : Type.Constructor.t Type_name.Map.t
  ; constructors : Type_name.t Constructor.Map.t
  ; fields : Type_name.t Field_name.Map.t
  ; type_classes : Type_class.t Type_class_name.Map.t
  ; type_class_implementations : Type_class.Impl.t list
  }
[@@deriving sexp_of]

let empty () =
  { values = Ident.Map.empty
  ; type_declarations =
      Intrinsic.Type.all
      |> List.map ~f:(fun intrinsic ->
        ( Intrinsic.Type.type_name intrinsic
        , { Type.Constructor.shape = Intrinsic intrinsic
          ; args =
              List.init (Intrinsic.Type.arity intrinsic) ~f:(fun _ -> Type.Var.create ())
          ; loc = Span.dummy
          } ))
      |> Type_name.Map.of_alist_exn
  ; constructors = Constructor.Map.empty
  ; fields = Field_name.Map.empty
  ; type_classes = Type_class_name.Map.empty
  ; type_class_implementations = []
  }
;;

let field t name ~loc =
  Map.find t.fields name
  |> Result.of_option
       ~error:(Type_error.of_string ~loc [%string "Unbound field [%{name#Field_name}]"])
;;

let constructor t name ~loc =
  Map.find t.constructors name
  |> Result.of_option
       ~error:
         (Type_error.of_string ~loc [%string "Unbound constructor %{name#Constructor}"])
;;

let type_declaration t name ~loc =
  Map.find t.type_declarations name
  |> Result.of_option
       ~error:(Type_error.of_string ~loc [%string "Unbound type %{name#Type_name}"])
;;

let value t name ~loc =
  Map.find t.values name
  |> Result.of_option
       ~error:(Type_error.of_string ~loc [%string "Unbound variable [%{name#Ident}]"])
;;

let type_class t name ~loc =
  Map.find t.type_classes name
  |> Result.of_option
       ~error:
         (Type_error.of_string
            ~loc
            [%string "Unknown type class [%{name#Type_class_name}]"])
;;

let with_fields t fields ~type_name =
  let%bind.Result fields =
    List.fold_result
      fields
      ~init:t.fields
      ~f:(fun fields ({ Located.value = name; loc }, _) ->
        match Map.add fields ~key:name ~data:type_name with
        | `Ok fields -> Ok fields
        | `Duplicate ->
          Error
            (Type_error.of_string ~loc [%string "Duplicate field [%{name#Field_name}]"]))
  in
  Ok { t with fields }
;;

let with_constructors t constructors ~type_name =
  let%bind.Result constructors =
    List.fold_result
      constructors
      ~init:t.constructors
      ~f:(fun constructors ({ Located.value = name; loc }, _) ->
        match Map.add constructors ~key:name ~data:type_name with
        | `Ok fields -> Ok fields
        | `Duplicate ->
          Error
            (Type_error.of_string
               ~loc
               [%string "Duplicate constructor [%{name#Constructor}]"]))
  in
  Ok { t with constructors }
;;

let with_type_declaration t name ty =
  { t with type_declarations = Map.add_exn t.type_declarations ~key:name ~data:ty }
;;

let with_vars t vars =
  let values =
    List.fold vars ~init:t.values ~f:(fun env (name, var) ->
      Map.set
        env
        ~key:name
        ~data:{ ty = Var var; quantifiers = Type.Var.Set.empty; constraints = [] })
  in
  { t with values }
;;

let with_var t name ty = { t with values = Map.set t.values ~key:name ~data:ty }
let remove_var t name = { t with values = Map.remove t.values name }

let with_type_class t name tc =
  { t with type_classes = Map.add_exn t.type_classes ~key:name ~data:tc }
;;

let with_type_class_impl t tc =
  { t with type_class_implementations = tc :: t.type_class_implementations }
;;
