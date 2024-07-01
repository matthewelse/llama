open! Core

module Message = struct
  type t =
    | Duplicate_constructor
    | Duplicate_field
    | Duplicate_type_declaration
    | Parse_error of int
    | Type_error
    | Unbound_field
    | Unbound_constructor
    | Unbound_type_class
    | Unbound_type_constructor
    | Unbound_variable

  let default_severity : t -> Asai.Diagnostic.severity = function
    | Duplicate_constructor | Duplicate_field -> Warning
    | Duplicate_type_declaration
    | Parse_error _
    | Type_error
    | Unbound_field
    | Unbound_constructor
    | Unbound_type_class
    | Unbound_type_constructor
    | Unbound_variable -> Error
  ;;

  let short_code = function
    | Duplicate_constructor -> "E000"
    | Duplicate_field -> "E001"
    | Duplicate_type_declaration -> "E002"
    | Parse_error n -> [%string "E003:%{n#Int}"]
    | Type_error -> "E004"
    | Unbound_constructor -> "E005"
    | Unbound_field -> "E006"
    | Unbound_type_class -> "E007"
    | Unbound_type_constructor -> "E008"
    | Unbound_variable -> "E009"
  ;;
end

include Asai.Reporter.Make (Message)
