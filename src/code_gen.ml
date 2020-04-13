open Ast
open Llvm

exception Record_too_small

type t = { bindings : (string, Ast.expr) Hashtbl.t
         ; type_dict : (string, lltype) Hashtbl.t
         ; llvm_context : llcontext
         ; builder : llbuilder
         ; m : llmodule
         ; pm : [ `Function ] PassManager.t
         }

type env = (string, (Ast.typ * llvalue)) Hashtbl.t

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
    ; pm = PassManager.create_function m
  }

(* This is a stop-gap from Runtime.exec, needs to be re-thought.  *)
let new_env () = Hashtbl.create 10

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

(* When a function returns a record, the JIT'd version will actually expect a
   final synthetic parameter which is a pointer to an already allocated record
   (LLVM "structure").  This function names that parameter based on the bound
   function's name.
 *)
let synth_rec_return_name fun_name =
  "_" ^ fun_name ^ "_rec_return"
  
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
let typ_of deps env = function
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
  | Var n ->
     fst @@ Hashtbl.find env n
  | _ ->
     failwith "Can't find a type for this."

let rec code_gen ({ builder; _ } as deps) env = function
  | Var n ->
     snd @@ Hashtbl.find env n
  | (Record fields) as r ->
     let t = get_type deps (typ_of deps env r) in
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
       let llv = code_gen deps env v in
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
     stack_rec
  | Get_field (f_name, r_exp, _f_typ) ->
     begin
       match typ_of deps env r_exp with
       | (TRecord { members = ms; _ }) as typ_rec ->
          let index = List.sort (fun a b -> String.compare (fst a) (fst b)) ms
                      |> List.mapi (fun i (n, _) -> n, i)
                      |> List.assoc f_name
          in
          let r_val = code_gen deps env r_exp in
          dump_value r_val;
          let _aa = build_alloca (get_type deps typ_rec) "tmp_record_for_compare" builder in
          let ptr = build_struct_gep r_val index "ptr_rec_field" builder in
          build_load ptr "tmp_rec_field" builder
       | _ ->
          failwith "Unacceptable record expression."
     end
  | Int i ->
     const_of_int64 (get_type deps (TInt)) (Int64.of_int i) true
  | Apply (name, args) ->
     gen_apply deps env name args
  | _ ->
     failwith "Unsupported AST node for JIT code generation."

(* Generate an actual function application.  This will try to look up any
   previously generated target for [name], including monomorphized versions,
   before attempting to generate LLVM IR for what is needed at this application
   point.
*)
and gen_apply ({ m; builder; _ } as deps) env name args =
  match List.map (fun a -> typ_of deps env a) args
        |> lookup_fun deps name
  with
  | Some fn ->
     (* We know this is safe due to the above `lookup_fun`:  *)
     let target = Option.get (lookup_function fn m) in
     (* TODO:  code_gen can't expand the environment right now, but might later:  *)
     let ll_args = List.map (fun e -> code_gen deps env e) args
                   |> Array.of_list
     in
     build_call target ll_args "call_tmp" builder
  | None ->
     failwith ("Unable to find function for " ^ name)


(* Given a function in [expr] and a list of argument types that are going to be
   applied to it, specialize the record arguments in [expr] so that they
   precisely match those in [arg_types].  All specialized record arguments must
   be closed at the end of this (no row variable).
 *)
and specialize_proto expr arg_types =
  match expr with
  | Fun { args; body } ->
     if List.length args != List.length arg_types then
       failwith "Incorrect number of arguments to a function"
     else
       (* check arg against submitted type, expand records, reject mismatch. *)
       let rec f (n, a) b =
         match a, b with
         (* The actual row variable isn't checked right now.  This means that if
            two different record arguments have the same row variable as a
            constraint, specialization won't respect this.  My optimistic
            assumption is a well-behaved type checker has done this check before
            constructing the AST that reaches code generation.
          *)
         | TRecord { members = m1; row }, TRecord { members = m2; _ } ->
            let _ = if List.length m1 > List.length m2 then
                      raise Record_too_small
                    else
                      ()
            in

            (* Check that m2 covers m1 by recursing to specialize the each
               member.  This covers our equality check as well as specializing
               records within records.

               Deliberately shadowing m1 here and this provides no useful
               feedback when m2 is missing something required by m1.
             *)
            let m1 = List.map (fun (n, t) -> (f (n, t) (List.assoc n m2))) m1 in
            (* Strip m1 members from m2, leaving only the ones that fit into the
               row variable:
             *)
            let m2 = List.fold_left (fun acc (n, _) -> List.remove_assoc n acc) m2 m1 in
            if List.length m2 > 0 && Option.is_none row then
              failwith ("Record argument " ^ n ^ " is not open (no row variable).")
            else
              (* Composite record:  *)
              (n, TRecord { members = List.append m1 m2; row = None })

         | a, b when a = b ->
            (n, a)
         | _, _ ->
            failwith "Function and argument types do not match."
       in
       Fun { args = List.map2 f args arg_types; body }
  | _ ->
     failwith "Can only specialize functions."

(* Check to see if a specialized function has aleady been generated in the
   LLVM module.
 *)
and lookup_fun ({ m; bindings; llvm_context = c; _ } as deps) name arg_types =
  let n = synth_fun_name name arg_types in

  let lookup_binding _ =
    match Hashtbl.find_opt bindings name with
    | None -> failwith (name ^ " is not bound")
    | Some expr -> expr
  in

  match lookup_function n m with
  | None ->
     let expr = lookup_binding () in
     let specialized = specialize_proto expr arg_types in
     (* Creating a new builder here may be *very* wasteful, not sure yet.
        The basic problem is nested generation of functions.  Builders seem to
        side-effect (internal state I guess, TBD) and when there are functions
        generated inside functions, the bottom function will acrete all the
        instructions generated by its (transitive?) caller(s), leading to
        errors.

        Possible change in direction:  generate stub specializations that when
        called, halt with an exception or value that the Runtime module can trap
        to JIT the desired specialization.  This would prevent going too deep
        into the call graph up front, and defer compilation.

        RELATED:  some ORC (the MCJIT replacement) material I've read suggests
        it allows for more control over laziness.  Maybe worth me digging into
        that to see if there's more control and simplification available.
      *)
     let _ = bind_gen { deps with builder = builder c } (Bind (n, specialized)) in
     Some n
  (*     failwith "No generation of functions yet." *)
  | Some _ ->
     Some n

(* Generate a binding's function prototype in the LLVM module.  *)
and gen_bind_proto ({m; llvm_context; _ } as deps) name { args; body = (ret_typ, _) } =
  (* TODO:  add final param if this function returns a record.  *)
  let ll_ret_typ = match ret_typ with
    | TRecord _ -> void_type llvm_context
    | _ -> get_type deps ret_typ
  in

  let args_arr = List.map (fun (_, t) ->
                     let tt = get_type deps t in
                     match t with
                     | TRecord _ -> pointer_type tt
                     | _ -> tt
                   ) args
                 |> Array.of_list
  in
  let ft = function_type ll_ret_typ args_arr in
  declare_function name ft m

(* Given an { Ast.bind }, generate LLVM IR for it in the module we're using for
   code generation.

   Functions that return records are rewritten to accept an additional argument
   from the caller, which is a pointer to an already-allocated structure in the
   caller's control.  The rewritten function will use the llvm.memcpy intrinsic
   to overwrite the struct from the caller as its last operation before
   returning.
 *)
and bind_gen ({ m; builder = b; llvm_context = c; pm; _ } as deps) = function
  | Bind (name, Fun ({ args; body = (ret_typ, expr) } as fr)) ->
     let ret_rec = synth_rec_return_name name in
     let args = match ret_typ with
       | TRecord _ -> List.append args [(ret_rec, ret_typ)]
       | _ -> args
     in

     let f = gen_bind_proto deps name { fr with args } in
     let env = Hashtbl.create 10 in
     let name_arr = List.map (fun (n, _) -> n) args |> Array.of_list in
     Array.iter2 (fun n p ->
         Hashtbl.add env n (List.assoc n args, p);
         set_value_name n p
       ) name_arr (params f);
     let bb = append_block c "entry" f in
     position_at_end bb b;
     let body = code_gen deps env expr in

     let _ = begin
         (* If the _original_ function in the AST returns a record, we rewrite
            it to use the final (and synthetic) pointer to a struct that the
            caller sent in order to receive the return value.
          *)
         match ret_typ with
         | TRecord _ ->
            (* This should maybe just always go into the module.
               TODO:  move it?
             *)
            let memcpy_typ = function_type
                               (void_type c)
                               (Array.of_list
                                  [ pointer_type (i8_type c)
                                  ; pointer_type (i8_type c)
                                  ; (i64_type c)
                                  ; (i1_type c)
                                  ]
                               )
            in
            let memcpy = declare_function "llvm.memcpy.p0i8.p0i8.i64" memcpy_typ m in
            let var = snd @@ Hashtbl.find env ret_rec in
            (* llvm.memcpy needs to know how big our struct/record is:  *)
            let size = size_of (get_type deps ret_typ) in
            let size_i = build_ptrtoint size (i64_type c) "tmp_struct_size" b in
            (* The llvm.memcpy intrinsic needs i8* arguments for source and
               destination so bitcast apropriately:
             *)
            let bc_var = build_bitcast var (pointer_type (i8_type c)) "bc_var" b in
            let bc_body = build_bitcast body (pointer_type (i8_type c)) "bc_body" b in
            let memcpy_args = Array.of_list [ bc_var
                                            ; bc_body
                                            ; size_i
                                            ; const_int (i1_type c) 0
                                ]
            in
            let _call = build_call memcpy memcpy_args "" b in
            build_ret_void b
         | _ ->
            build_ret body b
       end
     in
     Llvm_analysis.assert_valid_function f;
     let _ = PassManager.run_function f pm in
     f
  | _ ->
     failwith "Unsupported expression to bind."

let with_mod { m; _ } f = f m

(*
   In-line tests for specializing function prototypes.

   Inline because I didn't want to expose specialize_proto from this module.
 *)
let%test "Specializing to wrong types should fail" =
  try
    let _ = specialize_proto (c_fun [c_arg "x" TInt] (TInt, Var "x")) [TFun] in
    false
  with
    Failure _ -> true

let%test "Specializing matching records should pass" =
  (* Need this to get the type of arg1 later:  *)
  let cg = create [] in
  (* Throwaway function body here, just checking specialization:  *)
  let f = c_fun [("r", c_rectyp [("x", TInt)] (Some "r"))] (TInt, Int 1) in
  let arg1 = Record [c_field "x" TInt (Int 1)] in
  let t_arg1 = typ_of cg (Hashtbl.create 0) arg1 in
  let res = specialize_proto f [t_arg1] in
  let expected_typ = c_rectyp [("x", TInt)] None in
  match res with
  | Fun { args = [(_, res_arg1)]; _ } ->
     t_arg1 = res_arg1 && t_arg1 = expected_typ
  | _ ->
     false

let%test "Specializing to a wider record should grow the type" =
  (* Need this to get the type of arg1 later:  *)
  let cg = create [] in
  (* Throwaway function body here, just checking specialization:  *)
  let f = c_fun [("r", c_rectyp [("x", TInt)] (Some "r"))] (TInt, Int 1) in
  let arg1 = Record [c_field "x" TInt (Int 1); c_field "y" TInt (Int 2)] in
  let t_arg1 = typ_of cg (Hashtbl.create 0) arg1 in
  let res = specialize_proto f [t_arg1] in
  [%test_match? (Fun { args = [( "r"
                               , TRecord { members = [ ("x", TInt)
                                                     ; ("y", TInt)
                                                     ]
                                         ; row = None } )]
                     ; _ })]
    res;
  true

let%test "Trying to specialize to a smaller record should fail." =
  (* Need this to get the type of arg1 later:  *)
  let cg = create [] in
  (* Throwaway function body here, just checking specialization:  *)
  let f =
    c_fun [("r", c_rectyp [("x", TInt); ("y", TInt)] (Some "r"))] (TInt, Int 1)
  in
  let arg1 = Record [c_field "x" TInt (Int 1)] in
  let t_arg1 = typ_of cg (Hashtbl.create 0) arg1 in
  try let _ = specialize_proto f [t_arg1] in false with Record_too_small -> true

let%test "Specializing a TInt -> TRecord -> TInt function only touches the record." =
  (* Need this to get the type of arg1 later:  *)
  let cg = create [] in
  (* Throwaway function body here, just checking specialization:  *)
  let f =
    c_fun [ ("a", TInt)
          ; ("r", c_rectyp [("x", TInt)] (Some "r"))]
      (TInt, Int 1)
  in
  let arg1 = Int 1 in
  let arg2 = Record [c_field "x" TInt (Int 1); c_field "y" TInt (Int 2)] in
  let env = Hashtbl.create 0 in
  let t_arg1 = typ_of cg env arg1 in
  let t_arg2 = typ_of cg env arg2 in
  let res = specialize_proto f [t_arg1; t_arg2] in
  [%test_match? Fun { args = [ ("a", TInt)
                             ; ("r", TRecord { members = [ ("x", TInt)
                                                         ; ("y", TInt)
                                                         ]
                                             ; row = None })]
                    ; body = _ }]
    res;
  true
