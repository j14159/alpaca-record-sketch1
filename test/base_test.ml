open Jit
open Jit.Ast
open Jit.Interp

open OUnit2

let test_interp_get_field _ =
  let record = Record [{ field_name = "x"
                       ; typ = TInt
                       ; v = Int 1}]
  in
  let res = interp_eval [] (Get_field ("x", record, TInt)) in
  assert_equal (Int 1) res

type jit_record
  
let test_jit_record _ =
  let rt = Runtime.create [] in
  let record = Record [{ field_name = "x"
                       ; typ = TInt
                       ; v = Int 1 }]
  in

  (* The expected return type.  *)
  let open Ctypes in
  let jit_record : jit_record structure typ = structure "jit_record" in
  let jit_record_x = field jit_record "x" Ctypes.long in
  seal jit_record;

  let res = make jit_record in
  let _ = Runtime.exec ~dump_module:true rt record (Ctypes.ptr jit_record) jit_record (addr res) in

  (* Extract the x field from the return value.  *)
  let x = getf res jit_record_x in
  assert_equal (Int64.of_int 1) (Signed.Long.to_int64 x) ~printer:Int64.to_string

let test_int_identity _ =
  let int_id = Bind ("int_id", c_fun [("x", TInt)] (TInt, Var "x")) in
  let expr = Apply ("int_id", [Int 13]) in
  let rt = Runtime.create ~mod_name:"int_identity" [int_id] in
  assert_equal (Int64.of_int 13) (Runtime.exec rt expr Ctypes.void Ctypes.int64_t ())

let test_get_field _ =
  let get_x = Bind ( "get_x"
                   , Fun { args = [("r", TRecord { members = [("x", TInt)]; row = Some "r" })]
                         ; body = (TInt, Get_field ("x", Var "r", TInt))
                })
  in
  let expr = Apply ("get_x", [Record [ { field_name = "x"; typ = TInt; v = Int 5 }
                                     ; { field_name = "y"; typ = TInt; v = Int 12 }
               ]]) in
  let rt = Runtime.create [get_x] in
  let res = Runtime.exec ~dump_module:false rt expr Ctypes.void Ctypes.int64_t () in
  (* TODO:  send another record, check for correct specializations.  *)
  assert_equal (Int64.of_int 5) res ~printer:Int64.to_string  
  
let suite =
  "Base tests to iterate with" >:::
    [ "Intertpreter field get" >:: test_interp_get_field
    ; "MCJIT create record/structure." >:: test_jit_record
    ; "MCJIT get a field with a JIT compiled function" >:: test_get_field
    ; "MCJIT execute an integer identity function" >:: test_int_identity
    ]
   
let _ =
  run_test_tt_main suite
