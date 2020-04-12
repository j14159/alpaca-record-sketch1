type expr = Record of record_member list
          | Int of int
          | Var of string
          (* This should also have type variables up front so that record row
             variables can be properly reused.  This would also be a
             prerequisite for "ordinary" generics (System-F with prenex).
           *)
          | Fun of { args : (string * typ) list
                   ; body : (typ * expr)
                   }
          | Apply of string * (expr list)
          | Get_field of expr * string
and record_member = { field_name : string
                    ; typ : typ
                    ; v : expr
                    }
(* Allowing for replacement of row variable.  This is _much_ too
   over-simplified but it will work for this limited experiment.
 *)
and typ = TRecord of { members : (string * typ) list
                     ; row : string option
                     }
        | TInt
        | TFun

(* Intending only function support for now.  *)
type bind = Bind of string * expr

let c_field field_name typ v = { field_name; typ; v }
let c_rectyp members row = TRecord { members; row }
let c_fun args body = Fun { args; body }
let c_arg name t = (name, t)
