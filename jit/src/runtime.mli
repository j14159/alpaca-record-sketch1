open Llvm

type t

(* Mirrors from Code_gen since underneath it'll have a Code_gen.t
 *)
val create : ?context : llcontext -> ?mod_name : string -> Ast.bind list -> t

(* JIT compile and execute the given expression in the context of the current
   module.
 *)
val exec : ?name : string -> ?dump_module : bool -> t -> Ast.expr -> 'a Ctypes.typ -> 'b Ctypes.typ -> 'a -> 'b

val bind : t -> Ast.bind -> 'a Ctypes.typ -> (float * 'a)

val with_module : t -> (llmodule -> 'a) -> 'a
