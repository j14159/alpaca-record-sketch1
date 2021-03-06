open Llvm

type t

(* Environments for code generation.

   These track local variables and their types.
 *)
type env

val create : ?context : llcontext -> ?m : llmodule -> ?pm:[`Module] PassManager.t -> Ast.bind list -> t

val new_env : unit -> env

(* Given an {! Ast.typ } this will try to find an existing {! Llvm.lltype } 
   before creating a new one.
 *)
val get_type : t -> Ast.typ -> lltype

(** Generate LLVM IR for the provided [Ast.expr].
    
    This will trigger specialization for any functions that deal with records.

    The [no_pointer] option should be [true] if the code generator should not
    return a pointer to the result.  This is important for record operations
    like {! Ast.Get_field }.  Getting a field from a record (LLVM struct)
    results in a pointer to the item.  This is good if a record is being
    returned, but bad if the return is being passed to something like integer
    addition.

    This is a bit of a kludge atm and should be re-thought.

    More broadly, TODO:  rethink (actually think about?) the memory model.
 *)
val code_gen : ?no_pointer : bool -> t -> env -> Ast.expr -> llvalue

(** Bind an expression to a name in the current context (sort of a module).

    Currently this will only accept functions.
 *)
val bind_gen : t -> Ast.bind -> llvalue

(** Get the type of an expression based on the currently available bindings.
 *)
val typ_of : t -> env -> Ast.expr -> Ast.typ

(* Used to debug in tests.  *)
val with_mod : t -> (llmodule -> 'a) -> 'a
