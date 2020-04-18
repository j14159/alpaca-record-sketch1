open Jit
open Jit.Ast
open Jit.Interp

open OUnit2

let test_interp_get_field _ =
  let record = Record [{ field_name = "x"
                       ; typ = TInt
                       ; v = Int 1}]
  in
  let res = interp_eval (Interp.create []) (Hashtbl.create 0) (Get_field ("x", record, TInt)) in
  assert_equal (Int 1) res

let suite =
  "Base tests to iterate with" >:::
    [ "Intertpreter field get" >:: test_interp_get_field
    ]
   
let _ =
  run_test_tt_main suite
