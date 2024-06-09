open! Core

include
  String_id.Make_with_validate
    (struct
      let module_name = "Constructor"

      let validate s =
        if String.is_empty s
        then Or_error.error_string "Constructor name cannot be empty"
        else if not (Char.is_uppercase s.[0])
        then Or_error.error_string "Constructor name must start with an uppercase letter"
        else Or_error.return ()
      ;;

      let include_default_validation = false
    end)
    ()
