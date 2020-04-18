open Jit
open Ast
open OUnit2

let test_add_compare _ =
  let jit_rt = Runtime.create [] in
  let int_rt = Interp.create [] in
  let program = Apply ("addi", [Int 5; Int 57]) in
  let jit_res = Runtime.exec jit_rt program Ctypes.void Ctypes.int64_t () in
  let int_res = Interp.exec int_rt program in
  assert_equal int_res (Int (Int64.to_int jit_res))

let suite =
  "Comparison test suite" >:::
    ["Addition by JIT and interpreter should match" >:: test_add_compare]

let _ =
  run_test_tt_main suite
