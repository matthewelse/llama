open! Core

module Message = struct
  type t =
    | Parse_error of int
    | Type_error

  let default_severity _ = Asai.Diagnostic.Error

  let short_code = function
    | Parse_error n -> [%string "E001:%{n#Int}"]
    | Type_error -> "E002"
  ;;
end

include Asai.Reporter.Make (Message)
