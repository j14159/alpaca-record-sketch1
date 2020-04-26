open Ast
open Llvm

type t = { cg : Code_gen.t
         ; m : llmodule
         ; pm : [`Module] PassManager.t
         ; engine : Llvm_executionengine.llexecutionengine
         }

let create
      ?context:(ctx = global_context ())
      ?mod_name:(mn = "a_module")
      bindings
  =
  let m = create_module ctx mn in
  assert (Llvm_executionengine.initialize ());
  let pm = PassManager.create () in
  let open Llvm_executionengine in
  (* I exposed optimization level to play around with this a bit but haven't
     seen a major impact with my limited test cases so far.
   *)
  let ee_opts = { default_compiler_options with opt_level = 0 } in
  let engine = Llvm_executionengine.create ~options:ee_opts m in
  { cg = Code_gen.create ~context:ctx ~m ~pm bindings
  ; m
  ; pm
  ; engine
  }

let exec ?name:(name="th") ?dump_module:(dm = false) { m; cg; pm; engine } expr arg_t ret_t arg =
  let body_typ = Code_gen.typ_of cg (Code_gen.new_env ()) expr in
  let ast = (Bind (name, Fun { args = []; body = (body_typ, expr) })) in
  let _thunk = Code_gen.bind_gen cg ast in
  (* There's some sort of re-indexing necessary that requires this:  *)
  Llvm_executionengine.remove_module m engine;
  Llvm_executionengine.add_module m engine;
  (* TODO:  make this conditional on something in `t`:  *)
  if dm then dump_module m else ();
  assert (not (PassManager.run_module m pm));
  let open Ctypes in
  let actual_ctype = Foreign.funptr (arg_t @-> (returning ret_t)) in
  let f = Llvm_executionengine.get_function_address name actual_ctype engine in
  f arg

let bind { cg; pm; engine; m } b typ =
  let start = Unix.gettimeofday () in
  let Bind (name, _) = b in
  let _f = Code_gen.bind_gen cg b in
  assert (not (PassManager.run_module m pm));
  (* There's some sort of re-indexing necessary that requires this:  *)
  Llvm_executionengine.remove_module m engine;
  Llvm_executionengine.add_module m engine;
  let f = Llvm_executionengine.get_function_address name typ engine in
  ((Unix.gettimeofday ()) -. start, f)

let with_module { m; _ } f = f m
