open Jit
open Ast

(* This is a bit silly but wanted to get a baseline on how far away from native
   OCaml compilation my naive JIT is.  I haven't yet investigated why the JIT
   version is so much slower than OCaml (roughly an order of magnitude
   difference).
 *)
module Case : Test_case.Test_case = struct
  let ast_double = Bind
                     ( "double"
                     , Fun { args = [("x", TInt)]
                           ; body = ( TInt
                                    , Apply ("addi", [(Var "x"); (Var "x")])
                                    )
                         }
                     )

  let ocaml_double x = Int64.add x x

  let jit_case () =
    let rt = Runtime.create [] in
    let open Ctypes in
    let ft = Foreign.funptr (Ctypes.int64_t @-> (returning Ctypes.int64_t)) in
    (* Forcing compilation:  *)
    Runtime.bind rt ast_double ft

  let interp_case () =
    let it = Interp.create [ast_double] in
    let f x = Interp.exec it (Apply ("double", [Int x])) in
    f

  let run count cases =
    let i64_bounds = Int64.of_int 1000000 in
    let int_bounds = 1000000 in

    let (t, jitf) = jit_case () in
    let intf = interp_case () in

    let i64s = Array.init cases (fun _ -> Random.int64 i64_bounds) in
    let ints = Array.init cases (fun _ -> Random.int int_bounds) in

    let rec bench c start f xs =
      if c < 0 then
        Unix.gettimeofday () -. start
      else
        begin
          let _ = f xs.(c mod cases) in
          bench (c - 1) start f xs
        end
    in

    let bbench f xs = bench count (Unix.gettimeofday ()) f xs in
    ( t
    , bbench jitf i64s
    , bbench intf ints
    , bbench ocaml_double i64s
    )
end
