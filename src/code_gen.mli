open Llvm

type t

(* Environments for code generation.

   These track local variables and their types.
 *)
type env

val create : ?context : llcontext -> ?m : llmodule -> Ast.bind list -> t

val new_env : unit -> env

(* Given an {! Ast.typ } this will try to find an existing {! Llvm.lltype } 
   before creating a new one.
 *)
val get_type : t -> Ast.typ -> lltype

(* Generate LLVM IR for the provided [Ast.expr].  *)
val code_gen : t -> env -> Ast.expr -> llvalue

val bind_gen : t -> Ast.bind -> llvalue

val typ_of : t -> env -> Ast.expr -> Ast.typ

(* Used to debug in tests.  *)
val with_mod : t -> (llmodule -> 'a) -> 'a
