open Llvm

type t

val create : ?context : llcontext -> ?mod_name : string -> Ast.bind list -> t

(* Given an {! Ast.typ } this will try to find an existing {! Llvm.lltype } 
   before creating a new one.
 *)
val get_type : t -> Ast.typ -> lltype

(* Generate LLVM IR for the provided [Ast.expr].  *)
val code_gen : t -> Ast.expr -> llvalue

(* JIT compile and execute the given expression in the context of the current
   module.
 *)
val exec : ?name : string -> t -> Ast.expr -> 'a Ctypes.typ -> 'a

(* Used to debug in tests.  *)
val with_mod : t -> (llmodule -> 'a) -> 'a
