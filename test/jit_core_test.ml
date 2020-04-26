open Jit
open Jit.Ast

open OUnit2

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
  let _ = Runtime.exec ~dump_module:false rt record (Ctypes.ptr jit_record) jit_record (addr res) in

  (* Extract the x field from the return value.  *)
  let x = getf res jit_record_x in
  assert_equal (Int64.of_int 1) (Signed.Long.to_int64 x) ~printer:Int64.to_string

let test_int_identity _ =
  let int_id = Bind ("int_id", c_fun [("x", TInt)] (TInt, Var "x")) in
  let expr = Apply ("int_id", [Int 13]) in
  let rt = Runtime.create ~mod_name:"int_identity" [int_id] in
  assert_equal (Int64.of_int 13) (Runtime.exec rt expr Ctypes.void Ctypes.int64_t ())

(* Create a function that gets the value of the `x` field in a record conforming
   to the type { x : int | r }, where `r` is the row variable, and check that
   the JIT will specialize for at least two distinct types at "runtime".
 *)
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
  assert_equal (Int64.of_int 5) res ~printer:Int64.to_string;
  let open Llvm in
  let fs = Runtime.with_module
             rt
             (fun m -> fold_left_functions (fun a lv -> (value_name lv) :: a) [] m)
  in
  let str_list_printer xs = List.fold_left (fun a b -> a ^ "; " ^ b) "" xs in
  assert_equal
    [ "llvm.stackprotector"
    ; "get_x__record_xintyint"
    ; "th"
    (* Code_gen now adds this by default since we need it for many record
       operations:
     *)
    ; "llvm.memcpy.p0i8.p0i8.i64"
    ]
    fs
    ~printer:str_list_printer;
  (* Now run for a wider record again and check both result for correctness and
     for specialization results.

     Just shadowing expr and res here because I'm being lazy.
   *)
  let expr = Apply ("get_x", [Record [ { field_name = "x"; typ = TInt; v = Int 85 }
                                     ; { field_name = "y"; typ = TInt; v = Int 96 }
                                     ; { field_name = "z"; typ = TInt; v = Int 2 }
               ]]) in
  (* Currently `exec` creates a named "thunk", and reusing the name doesn't
     appear to overwrite the previously defined one.
   *)
  let res = Runtime.exec ~name:"th2" rt expr Ctypes.void Ctypes.int64_t () in
  assert_equal (Int64.of_int 85) res ~printer:Int64.to_string;
  (* Get all the function names again.  *)
  let fs = Runtime.with_module
             rt
             (fun m -> fold_left_functions (fun a lv -> (value_name lv) :: a) [] m)
  in
  assert_equal
    [ "get_x__record_xintyintzint"
    ; "th2"
    ; "llvm.stackprotector"
    ; "get_x__record_xintyint"
    ; "th"
    ; "llvm.memcpy.p0i8.p0i8.i64"
    ]
    fs
    ~printer:str_list_printer

let test_add _ =
  let rt = Runtime.create [] in
  let res = Runtime.exec rt (Apply ("addi", [Int 1; Int 2])) Ctypes.void Ctypes.int64_t () in
  assert_equal (Int64.of_int 3) res

let test_nested _ =
  let rt = Runtime.create [] in
  let nested_rec_type = c_rectyp [("x", TInt); ("y", TInt)] None in
  let _rec_type = c_rectyp
                   [ ("z", TInt)
                   ; ("r1", nested_rec_type)
                   ]
                   None
  in
  let rec_v = Record
                [ c_field "z" TInt (Int 1)
                ; c_field "r1" nested_rec_type (Record [ c_field "x" TInt (Int 2)
                                                       ; c_field "y" TInt (Int 3)])
                ]
  in
  let p = Get_field ("x", (Get_field ("r1", rec_v, nested_rec_type)), TInt) in
  let res = Runtime.exec ~name:"test_nested thunk" rt p Ctypes.void Ctypes.int64_t () in
  assert_equal (Int64.of_int 2) res ~printer:Int64.to_string

let test_double_record_field _ =
  let get_x = Get_field ("an_int", Var "r", TInt) in
  let arg_type = c_rectyp [("an_int", TInt)] (Some "row") in
  let double = Bind ("double", c_fun
                                 [c_arg "r" arg_type]
                                 (TInt, (Apply ("addi", [get_x; get_x])))
                 )
  in
  let rt = Runtime.create [double] in
  let program1 = (Apply ("double", [Record [c_field "an_int" TInt (Int 12)]])) in
  let res1 = Runtime.exec ~name:"run1" rt program1 Ctypes.void Ctypes.int64_t () in
  let program2 = (Apply ("double", [Record [ c_field "zzz" TInt (Int 35)
                                           ; c_field "an_int" TInt (Int 4)]]))
  in
  let res2 = Runtime.exec ~name:"run2" rt program2 Ctypes.void Ctypes.int64_t () in
  assert_equal (Int64.of_int 24) res1 ~printer:Int64.to_string;
  assert_equal (Int64.of_int 8) res2 ~printer:Int64.to_string

(* Make sure using the result of a function application to fill in a record's
   fields will work.
 *)
let test_record_construction_with_apply _ =
  let double = Bind ("double", Fun { args = [("x", TInt)]
                                   ; body = (TInt, Apply ("addi", [Var "x"; Var "x"]))
                 })
  in
  let rt = Runtime.create [double] in
  let r = Record
            [ { field_name = "x"; typ = TInt; v = Int 1 }
            ; { field_name = "z"; typ = TInt; v = Apply ("addi", [Int 12; Int 12]) }
            ; { field_name = "y"; typ = TInt; v = Apply ("double", [Int 2]) }
            ]
  in
  let program1 = Get_field ( "z", r, TInt) in
  let program2 = Get_field ("y", r, TInt) in
  let res1 = Runtime.exec ~name:"p1" rt program1 Ctypes.void Ctypes.int64_t () in
  let res2 = Runtime.exec ~name:"p2" rt program2 Ctypes.void Ctypes.int64_t () in
  assert_equal (Int64.of_int 24) res1 ~printer:Int64.to_string;
  assert_equal (Int64.of_int 4) res2 ~printer:Int64.to_string

(* Make sure nested polymorphic records are also compiled correctly.  *)
let test_nested_polymorphic_records _ =
  let internal_rec_type = c_rectyp [("abc", TInt)] (Some "inner_row") in
  let outer_rec_type = c_rectyp [("internal_rec", internal_rec_type)] (Some "outer_row") in
  let get_abc_body = ( TInt
                     , Get_field ( "abc"
                                 , (Get_field ("internal_rec", Var "r", internal_rec_type))
                                 , TInt)
                     )
  in
  let get_abc_args = [("r", outer_rec_type)] in
  let get_abc = Bind ("get_abc", c_fun get_abc_args get_abc_body) in
  (* TODO:  test actual results.  *)
  let rt = Runtime.create [get_abc] in

  let program1_irt = c_rectyp [("x", TInt); ("abc", TInt)] None in
  let program1_ir = Record
                      [ c_field "x" TInt (Int 3)
                      ; c_field "abc" TInt (Int 47)
                      ]
  in
  let program1 = Apply ("get_abc", [Record [c_field "internal_rec" program1_irt program1_ir]]) in


  let program2_irt = c_rectyp [("x", TInt); ("abc", TInt); ("_1", TInt)] None in
  let program2_ir = Record
                      [ c_field "x" TInt (Int 333)
                      ; c_field "abc" TInt (Int (-5))
                      ; c_field "_1" TInt (Int 0)
                      ]
  in
  let program2 = Apply
                   ("get_abc", [Record
                                      [ c_field "internal_rec" program2_irt program2_ir
                                      ; c_field "don't use" TInt (Int 512)
                                      ]
                               ]
                   )
  in

  let res1 = Runtime.exec ~name:"run1" rt program1 Ctypes.void Ctypes.int64_t () in
  let res2 = Runtime.exec ~name:"run2" rt program2 Ctypes.void Ctypes.int64_t () in
  (* TODO:  test presence of compiled methods.  *)
  assert_equal (Int64.of_int 47) res1 ~printer:Int64.to_string;
  assert_equal (Int64.of_int (-5)) res2 ~printer:Int64.to_string

let suite =
  "MCJIT code generation tests" >:::
    [ "Create record/structure." >:: test_jit_record
    ; "Get a field with a JIT compiled function" >:: test_get_field
    ; "Execute an integer identity function" >:: test_int_identity
    ; "Execute a basic integer addition" >:: test_add
    ; "Get a nested record's field" >:: test_nested
    ; "Double a flat record's int field" >:: test_double_record_field
    ; "Construct a record calling a function for a field" >:: test_record_construction_with_apply
    ; "Nested polymorphic records" >:: test_nested_polymorphic_records
    ]
   
let _ =
  run_test_tt_main suite
