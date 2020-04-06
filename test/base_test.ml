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

(* let test_jit_int _ =
  let deps = CodeGenDeps.of_context (Llvm.global_context ()) in
  let mm = ModManager.of_string deps "test_jit_int" in
  let thunk = ModManager.thunk mm (Int 1) in
  assert (not (ModManager.run_fun mm thunk));
  assert (Llvm_executionengine.initialize ());
  let engine = ModManager.with_mod mm (fun m -> Llvm_executionengine.create m) in
  let _ = ModManager.with_mod mm (fun m -> Llvm.dump_module m) in
  let open Ctypes in
  let ft = Foreign.funptr (Ctypes.void @-> (returning Ctypes.int64_t)) in
  let f = Llvm_executionengine.get_function_address "th" ft engine in
  let res = f () in
  assert_equal (Int64.of_int 1) res ~printer:Int64.to_string
 *)
  
let suite =
  "Base tests to iterate with" >:::
    [ "Intertpreter field get" >:: test_interp_get_field
    ; "MCJIT create record/structure." >:: test_jit_record
                                             (* ; "MCJIT create int." >:: test_jit_int *)
    ]
   
let _ =
  run_test_tt_main suite
