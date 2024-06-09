open! Core
open! Import

let%expect_test _ =
  let pp_tv n = Pretty_print.For_testing.pp_tv' Format.std_formatter n in
  pp_tv 0;
  [%expect {| 'a |}];
  pp_tv 25;
  [%expect {| 'z |}];
  pp_tv 26;
  [%expect {| 'aa |}];
  pp_tv 27;
  [%expect {| 'ab |}]
;;
