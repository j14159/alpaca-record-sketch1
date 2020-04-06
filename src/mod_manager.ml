open Ast
open Code_gen
open Llvm

(** JIT compiles functions on-demand based on expressions to execute.
 *)
module ModManager : sig
  type t
  val create : CodeGenDeps.t -> Ast.bind list -> t
  val with_mod : t -> (llmodule -> 'a) -> 'a
  val run_fun : t -> llvalue -> bool
  val lookup_fun : t -> string -> Ast.typ list -> string Option.t
  val exec : ?name : string -> t -> Ast.expr -> 'a Ctypes.typ -> 'a
end = struct
  type t = { pm : [ `Function ] PassManager.t
           ; m : llmodule
           ; deps : CodeGenDeps.t
           ; bindings : (string, Ast.expr) Hashtbl.t
           }

  let create deps bindings =
    let m = CodeGenDeps.with_ctx deps (fun ctx -> create_module ctx "a_module") in
    let pm = PassManager.create_function m in
    let bs = Hashtbl.create (List.length bindings) in
    List.iter (fun (Bind (name, fn)) -> Hashtbl.add bs name fn) bindings;
    { pm; m; deps; bindings = bs }

  let with_mod { m; _ } f = f m

  let run_fun { pm; _ } lv = PassManager.run_function lv pm

  let synth_fun_name name arg_types =
    let args_n = List.map (fun t -> CodeGenDeps.string_of_typ t) arg_types
                 |> List.fold_left (fun a b -> a ^ "_" ^ b) ""
    in
    name ^ "_" ^ args_n
    
  let lookup_fun { m; _ } name arg_types =
    let n = synth_fun_name name arg_types in
    match lookup_function n m with
    | None ->
       failwith "No generation of functions yet."
    | Some _ ->
       Some n
    

  let bind_gen { deps; m = a_mod; _ } = function
    | Bind (name, Fun { args; body = (ret_typ, expr) }) ->
       let ll_ret_typ = CodeGenDeps.get_type deps ret_typ in
       let args_arr = List.map (fun (_, t) -> CodeGenDeps.get_type deps t) args
                      |> Array.of_list
       in
       let ft = function_type ll_ret_typ args_arr in
       let f = declare_function name ft a_mod in
       let name_arr = List.map (fun (n, _) -> n) args |> Array.of_list in
       Array.iter2 (fun n p -> set_value_name n p) name_arr (params f);
       CodeGenDeps.with_builder deps
         (fun b ->
           CodeGenDeps.with_ctx deps
             (fun c ->
               let bb = append_block c "entry" f in
               position_at_end bb b;
               let body = code_gen deps expr in
               let _ = build_ret body b in
               f
             )
         )
    | _ ->
       failwith "Unsupported expression to bind."
      
  let exec ?name:(name="th") ({ m; _ } as deps) expr ctyp =
    let ast = (Bind (name, Fun { args = []; body = (typ_of expr, expr) })) in
    let thunk = bind_gen deps ast in
    assert (Llvm_executionengine.initialize ());
    assert (not (run_fun deps thunk));
    let engine = Llvm_executionengine.create m in
    let open Ctypes in
    let actual_ctype = Foreign.funptr (Ctypes.void @-> (returning ctyp)) in 
    let f = Llvm_executionengine.get_function_address name actual_ctype engine in
    f ()
end

        
