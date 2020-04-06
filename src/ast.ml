type expr = Record of record_member list
         | Int of int
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
