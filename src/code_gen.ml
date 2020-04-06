open Ast
open Llvm
   
module CodeGenDeps : sig
  type t
  val of_context : llcontext -> t
  val get_type : t -> typ -> lltype
  val with_builder : t -> (llbuilder -> llvalue) -> llvalue
  val with_ctx : t -> (llcontext -> 'a) -> 'a
  val string_of_typ : Ast.typ -> string
end = struct
  type t = { type_dict : (string, lltype) Hashtbl.t
           ; llvm_context : llcontext
           ; builder : llbuilder
           }

  let of_context ctx =
    { type_dict = Hashtbl.create 10
    ; llvm_context = ctx
    ; builder = builder ctx
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

  let with_builder { builder; _ } f =
    f builder

  let with_ctx { llvm_context; _ } f =
    f llvm_context
end
    
let typ_of = function
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
  | _ ->
     failwith "Not a value."
    
let rec code_gen deps = function
  | (Record fields) as r ->
     let t = CodeGenDeps.get_type deps (typ_of r) in
     let stack_rec = CodeGenDeps.with_builder
                       deps
                       (fun b -> Llvm.build_alloca t "tmprecord" b)
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
       in
       let _ =
         CodeGenDeps.with_builder
           deps
           (fun b -> let ptr = ptr b in Llvm.build_store llv ptr b)
       in
       ()
     in
     List.iteri f_iter sorted_fields;
     CodeGenDeps.with_builder deps (fun b -> build_load stack_rec "actual_rec" b)
  | Int i ->
     const_of_int64 (CodeGenDeps.get_type deps (TInt)) (Int64.of_int i) true
  | _ ->
     failwith "Unsupported AST node for JIT code generation."

and gen_apply _name _args =
  failwith "No apply JIT yet."    
