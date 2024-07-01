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
          ; loc = `Built_in
          } ))
      |> Type_name.Map.of_alist_exn
  ; constructors = Constructor.Map.empty
  ; fields = Field_name.Map.empty
  ; type_classes = Type_class_name.Map.empty
  ; type_class_implementations = []
  }
;;

let field_exn t name ~loc =
  match Map.find t.fields name with
  | None ->
    Reporter.fatal
      Unbound_field
      [%string "Unbound field [%{name#Field_name}]"]
      ~loc:(Asai.Range.of_lex_range loc)
  | Some x -> x
;;

let constructor_exn t name ~loc =
  match Map.find t.constructors name with
  | None ->
    Reporter.fatal
      Unbound_constructor
      [%string "Unbound constructor [%{name#Constructor}]"]
      ~loc:(Asai.Range.of_lex_range loc)
  | Some x -> x
;;

let type_declaration_exn t name ~loc =
  match Map.find t.type_declarations name with
  | None ->
    Reporter.fatal
      Unbound_type_constructor
      [%string "Unbound type declaration [%{name#Type_name}]"]
      ~loc:(Asai.Range.of_lex_range loc)
  | Some x -> x
;;

let value_exn t name ~loc =
  match Map.find t.values name with
  | None ->
    Reporter.fatal
      Unbound_variable
      [%string "Unbound variable [%{name#Ident}]"]
      ~loc:(Asai.Range.of_lex_range loc)
  | Some x -> x
;;

let type_class_exn t name ~loc =
  match Map.find t.type_classes name with
  | None ->
    Reporter.fatal
      Unbound_type_class
      [%string "Unbound type class [%{name#Type_class_name}]"]
      ~loc:(Asai.Range.of_lex_range loc)
  | Some x -> x
;;

let with_fields t fields ~type_name =
  let fields =
    List.fold fields ~init:t.fields ~f:(fun fields ((name, loc), _) ->
      match Map.add fields ~key:name ~data:type_name with
      | `Ok fields -> fields
      | `Duplicate ->
        Reporter.emit
          ~loc:(Asai.Range.of_lex_range loc)
          Duplicate_field
          [%string "Duplicate field [%{name#Field_name}] in scope."];
        fields)
  in
  { t with fields }
;;

let with_constructors t constructors ~type_name =
  let constructors =
    List.fold constructors ~init:t.constructors ~f:(fun constructors ((name, loc), _) ->
      match Map.add constructors ~key:name ~data:type_name with
      | `Ok constructors -> constructors
      | `Duplicate ->
        Reporter.emit
          ~severity:Warning
          ~loc:(Asai.Range.of_lex_range loc)
          Duplicate_constructor
          [%string "Duplicate constructor [%{name#Constructor}] in scope."];
        constructors)
  in
  { t with constructors }
;;

let with_type_declaration t name ty ~loc =
  let type_declarations =
    match Map.add t.type_declarations ~key:name ~data:ty with
    | `Ok type_declarations -> type_declarations
    | `Duplicate ->
      Reporter.emit
        ~severity:Warning
        ~loc:(Asai.Range.of_lex_range loc)
        Duplicate_type_declaration
        [%string "Duplicate type declaration [%{name#Type_name}] in scope."];
      t.type_declarations
  in
  { t with type_declarations }
;;

let with_vars t vars =
  let values =
    List.fold vars ~init:t.values ~f:(fun env (name, var) ->
      Map.set
        env
        ~key:name
        ~data:{ body = Var (var, ()); quantifiers = []; constraints = [] })
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
