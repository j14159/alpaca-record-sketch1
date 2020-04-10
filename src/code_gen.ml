open Ast
open Llvm

type t = { bindings : (string, Ast.expr) Hashtbl.t
         ; type_dict : (string, lltype) Hashtbl.t
         ; llvm_context : llcontext
         ; builder : llbuilder
         ; m : llmodule
         }

let create
      ?context:(ctx = global_context ())
      ?m:(m = create_module ctx "a_module")
      bindings =
  let bs = Hashtbl.create (List.length bindings) in
  List.iter (fun (Bind (name, expr)) ->
                  match expr with
                  | Fun _ ->
                     Hashtbl.add bs name expr
                  | _ ->
                     failwith "Only bind funs"
                 )
    bindings;
  { type_dict = Hashtbl.create 10
    ; llvm_context = ctx
    ; builder = builder ctx
    ; bindings = bs
    ; m
  }

(* I think this pair of functions is poorly thought-out. *)
let rec record_type_tag r =
  List.map (fun (field_name, typ) -> field_name ^ (string_of_typ typ)) r
  |> List.sort (String.compare)
  |> List.fold_left (fun a b -> a ^ b) ""
and string_of_typ = function
  | TRecord { members = ms; _ } -> "record_" ^ (record_type_tag ms)
  | TFun -> "fun"
  | TInt -> "int"

(* Make a synthetic function name that encodes the monomorphization of records.
 *)
let synth_fun_name name arg_types =
  let args_n = List.map (fun t -> string_of_typ t) arg_types
               |> List.fold_left (fun a b -> a ^ "_" ^ b) ""
  in
  name ^ "_" ^ args_n

let rec create_type ({ llvm_context; _ } as deps) t =
  match t with
  | TRecord { members; _ } ->
     let llvm_types = List.map (fun (_, typ) -> get_type deps typ) members in
     let arr = Array.of_list llvm_types in
     struct_type llvm_context arr
  | TInt ->
     i64_type llvm_context
  | _ ->
     failwith "Can't create this type yet."
and get_type ({ type_dict; _ } as deps) ast_typ =
  let tag = string_of_typ ast_typ in
  match Hashtbl.find_opt type_dict tag with
  | None ->
     let new_type = create_type deps ast_typ in
     Hashtbl.add type_dict tag new_type;
     new_type
  | Some existing_type ->
     existing_type

let get_binding { bindings; _ } name =
  Hashtbl.find_opt bindings name

(* Try to figure out an [Ast.typ] from a provided [Ast.expr].

   This needs a { t } so that it can look up module bindings.
*)
let typ_of deps = function
  | Record ms ->
     TRecord { members = (List.map
                            (fun { field_name; typ; _ } ->
                              (field_name, typ)
                            )
                            ms)
             ; row = Option.none
       }
  | Fun _ ->
     TFun
  | Int _ ->
     TInt
  | Apply (name, _) ->
     begin
       match get_binding deps name with
       | Some (Fun { body = (typ, _); _ }) -> typ
       | _ -> failwith (name ^ "is not bound to a function.")
     end
  | _ ->
     failwith "Can't find a type for this."

let rec code_gen ({ builder; _ } as deps) = function
  | (Record fields) as r ->
     let t = get_type deps (typ_of deps r) in
     let stack_rec = Llvm.build_alloca t "tmprecord" builder
     in
     let sorted_fields =
       List.sort
         (fun { field_name = f1; _ }
              { field_name = f2; _ }
          -> String.compare f1 f2)
         fields
     in
     let f_iter idx { v; _ } =
       let llv = code_gen deps v in
       let ptr =
         Llvm.build_struct_gep
           stack_rec
           idx
           ("struct_ptr" ^ (string_of_int idx))
           builder
       in
       let _ = Llvm.build_store llv ptr builder in
       ()
     in
     List.iteri f_iter sorted_fields;
     build_load stack_rec "actual_rec" builder
  | Int i ->
     const_of_int64 (get_type deps (TInt)) (Int64.of_int i) true
  | Apply (name, args) ->
     gen_apply deps name args
  | _ ->
     failwith "Unsupported AST node for JIT code generation."

(* Generate an actual function application.  This will try to look up any
   previously generated target for [name], including monomorphized versions,
   before attempting to generate LLVM IR for what is needed at this application
   point.
*)
and gen_apply deps name args =
  match List.map (fun a -> typ_of deps a) args
        |> lookup_fun deps name
  with
  | _ ->
     failwith "No apply JIT yet."

(* Check to see if a monomorphized function has aleady been generated in the
   LLVM module.
 *)
and lookup_fun { m; _ } name arg_types =
  let n = synth_fun_name name arg_types in
  match lookup_function n m with
  | None ->
     failwith "No generation of functions yet."
  | Some _ ->
     Some n

(* Given an { Ast.bind }, generate LLVM IR for it in the module we're using for
   code generation.
 *)
let bind_gen ({ m = a_mod; builder = b; llvm_context = c; _ } as deps) = function
  | Bind (name, Fun { args; body = (ret_typ, expr) }) ->
     let ll_ret_typ = get_type deps ret_typ in
     let args_arr = List.map (fun (_, t) -> get_type deps t) args
                    |> Array.of_list
     in
     let ft = function_type ll_ret_typ args_arr in
     let f = declare_function name ft a_mod in
     let name_arr = List.map (fun (n, _) -> n) args |> Array.of_list in
     Array.iter2 (fun n p -> set_value_name n p) name_arr (params f);
     let bb = append_block c "entry" f in
     position_at_end bb b;
     let body = code_gen deps expr in
     let _ = build_ret body b in
     f
  | _ ->
     failwith "Unsupported expression to bind."

let with_mod { m; _ } f = f m
