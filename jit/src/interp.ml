open Ast

type t = { bindings : (string, expr) Hashtbl.t }
type env = (string * expr) list

(**
   Limit to record creation, field access, integer values.
 *)
                         
let create bs =
  let bindings = Hashtbl.create (List.length bs) in
  let f = function
    | Bind (name, ((Fun _) as fn)) -> Hashtbl.add bindings name fn
    | _ -> failwith "Only function bindings supported."
  in
  List.iter f bs;
  { bindings }

let interp_get_field fields name =
  List.filter
    (fun { field_name; _ } -> field_name = name)
    fields
  |> List.map (fun { v; _ } -> v)
  |> List.hd 

let rec interp_eval ({ bindings; _ } as t) env ast_node =
  match ast_node with
  | Var v ->
     Hashtbl.find env v
  | Int _ ->
     ast_node
  | Get_field (name, expr, _typ) ->
     begin
       match interp_eval t env expr with
       | Record fields ->
          interp_eval t env (interp_get_field fields name)
       | _ ->
          failwith "Can't get a field from a non-record."
     end
  | Record ms ->
     let f = function
       | { v; _ } as r -> { r with v = interp_eval t env v }
     in
     Record (List.map f ms)
  | Apply ("addi", [arg1; arg2]) ->
     begin
       match (interp_eval t env arg1), (interp_eval t env arg2) with
       | Int a, Int b -> Int (a + b)
       | _ -> failwith "Type error for integer addition."
     end
  | Apply (fn, args) ->
     let next_env = Hashtbl.create (List.length args) in
     begin
       match Hashtbl.find bindings fn with
       | Fun { args = fn_args; body = (_, body) } ->
          let arg_ns = List.map (fun (n, _) -> n) fn_args in
          List.iter2
            (fun n a -> Hashtbl.add next_env n (interp_eval t env a))
            arg_ns
            args;
          interp_eval t next_env body
       | _ -> failwith "Can only apply to functions"
     end
  | _ ->
     failwith "Unsupported interpreter operation."

let exec t expr =
  interp_eval t (Hashtbl.create 5) expr
