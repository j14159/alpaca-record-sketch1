open Jit.Mod_manager
open Jit.Ast
open Jit.Code_gen
open Jit.Interp

open OUnit2


let test_interp_get_field _ =
  let record = Record [{ field_name = "x"
                       ; typ = TInt
                       ; v = Int 1}]
  in
  let res = interp_eval [] (Get_field (record, "x")) in
  assert_equal (Int 1) res

type jit_record
  
let test_jit_record _ =
  let deps = CodeGenDeps.of_context (Llvm.global_context ()) in
  let mm = ModManager.create deps [] in
  let record = Record [{ field_name = "x"
                       ; typ = TInt
                       ; v = Int 1 }]
  in

  (* The expected return type.  *)
  let open Ctypes in
  let jit_record : jit_record structure typ = structure "jit_record" in
  let jit_record_x = field jit_record "x" Ctypes.long in
  seal jit_record;

  let _ = ModManager.with_mod mm (fun m -> Llvm.dump_module m) in
  let res = ModManager.exec mm record jit_record in

  (* Extract the x field from the return value.  *)
  let x = getf res jit_record_x in
  assert_equal (Int64.of_int 1) (Signed.Long.to_int64 x) ~printer:Int64.to_string

let test_get_field _ =
  let get_x = Bind ( "get_x"
                   , Fun { args = [("r", TRecord { members = [("x", TInt)]; row = Option.none })]
                         ; body = (TInt, Get_field (Var "r", "x"))
                })
  in
  let expr = Apply ("get_x", [Record [{ field_name = "x"; typ = TInt; v = Int 5 }]]) in
  let mm = ModManager.create (CodeGenDeps.of_context (Llvm.global_context ())) [get_x] in
  let res = ModManager.exec mm expr (Ctypes.int64_t) in
  assert_equal (Int64.of_int 5) res ~printer:Int64.to_string  
  
let suite =
  "Base tests to iterate with" >:::
    [ "Intertpreter field get" >:: test_interp_get_field
    ; "MCJIT create record/structure." >:: test_jit_record
    ; "MCJIT get a field with a JIT compiled function" >:: test_get_field
    ]
   
let _ =
  run_test_tt_main suite
