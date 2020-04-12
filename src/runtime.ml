open Ast
open Llvm

type t = { cg : Code_gen.t
         ; m : llmodule
         }

let create
      ?context:(ctx = global_context ())
      ?mod_name:(mn = "a_module")
      bindings
  =
  let m = create_module ctx mn in
  { cg = Code_gen.create ~context:ctx ~m bindings;
    m
  }

let exec ?name:(name="th") ?dump_module:(dm = false) { m; cg } expr ctyp =
  let ast = (Bind (name, Fun { args = []; body = (Code_gen.typ_of cg expr, expr) })) in
  let thunk = Code_gen.bind_gen cg ast in
  (* TODO:  make this conditional on something in `t`:  *)
  if dm then dump_module m else ();
  let pm = PassManager.create_function m in
  assert (Llvm_executionengine.initialize ());
  assert (not (PassManager.run_function thunk pm));
  let engine = Llvm_executionengine.create m in
  let open Ctypes in
  let actual_ctype = Foreign.funptr (Ctypes.void @-> (returning ctyp)) in
  let f = Llvm_executionengine.get_function_address name actual_ctype engine in
  f ()
