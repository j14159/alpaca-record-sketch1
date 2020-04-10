open Llvm

type t

val create : ?context : llcontext -> ?m : llmodule -> Ast.bind list -> t

(* Given an {! Ast.typ } this will try to find an existing {! Llvm.lltype } 
   before creating a new one.
 *)
val get_type : t -> Ast.typ -> lltype

(* Generate LLVM IR for the provided [Ast.expr].  *)
val code_gen : t -> Ast.expr -> llvalue

val bind_gen : t -> Ast.bind -> llvalue

val typ_of : t -> Ast.expr -> Ast.typ

(* Used to debug in tests.  *)
val with_mod : t -> (llmodule -> 'a) -> 'a
