open Ast

(**
   Limit to record creation, field access, integer values.
 *)
                         
let interp_get_field fields name =
  List.filter
    (fun { field_name; _ } -> field_name = name)
    fields
  |> List.map (fun { v; _ } -> v)
  |> List.hd 

let interp_eval _env ast_node =
  match ast_node with
  | Get_field (name, Record fields, _typ) ->
     interp_get_field fields name
  | _ ->
     failwith "Unsupported interpreter operation."
