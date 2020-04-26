type expr = Record of record_member list
          | Int of int
          | Var of string
          (* This should also have type variables up front so that record row
             variables can be properly reused.  This would also be a
             prerequisite for "ordinary" generics (System-F with prenex).
           *)
          | Fun of fun_rec
          | Apply of string * (expr list)
          | Get_field of string * expr * typ
[@@deriving show]
and record_member = { field_name : string
                    ; typ : typ
                    ; v : expr
                    }
[@@deriving show]
and fun_rec = { args : (string * typ) list
              ; body : (typ * expr)
              }
[@@deriving show]
(* Allowing for replacement of row variable.  This is _much_ too
   over-simplified but it will work for this limited experiment.

   The row variable is vestigial, it serves essentially no useful purpose right
   now and could be removed.
 *)
and typ = TRecord of { members : (string * typ) list
                     ; row : string option
                     }
        | TInt
        | TFun
[@@deriving show]

(* Intending only function support for now.  *)
type bind = Bind of string * expr
[@@deriving show]

let c_field field_name typ v = { field_name; typ; v }
let c_rectyp members row = TRecord { members; row }
let c_fun args body = Fun { args; body }
let c_arg name t = (name, t)
