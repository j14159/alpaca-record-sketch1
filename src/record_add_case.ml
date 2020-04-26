open Jit
open Ast

(* Add two records' integer fields together.  *)
module Case : Test_case.Test_case = struct
  let case1 =
  (*
    let to_call { x : int; y : int } = x + y
   *)
  let target = Bind
                 ( "to_call"
                 , Fun
                     { args = [("r", c_rectyp [("x", TInt); ("y", TInt)] (Some "row"))]
                     ; body = ( TInt
                              , Apply
                                  ( "addi"
                                  , [ (Get_field ("x", Var "r", TInt))
                                    ; (Get_field ("y", Var "r", TInt))]
                                  )
                              )
                     }
                 )
  in
  let root1 = Bind
                ( "root1"
                   , Fun
                       { args = [("x", TInt); ("y", TInt)]
                       ; body = ( TInt
                                , Apply
                                    ( "to_call"
                                    , [Record
                                         [ { field_name = "x"; typ = TInt; v = Var "x" }
                                         ; { field_name = "y"; typ = TInt; v = Var "y" }
                                         ; { field_name = "x+y"
                                           ; typ = TInt
                                           ; v = Apply
                                                   ( "addi"
                                                   , [Var "x"; Var "y"]
                                                   )
                                           }
                                         ]
                                      ]
                                    )
                                )
                       }
                )
  in
  let root2 = Bind
                ( "root2"
                   , Fun
                       { args = [("x", TInt); ("y", TInt)]
                       ; body = ( TInt
                                , Apply
                                    ( "to_call"
                                    , [Record
                                         [ { field_name = "x"; typ = TInt; v = Var "x" }
                                         ; { field_name = "y"; typ = TInt; v = Var "y" }
                                         ; { field_name = "x+y"
                                           ; typ = TInt
                                           ; v = Apply
                                                   ( "addi"
                                                   , [Var "x"; Var "y"]
                                                   )
                                           }
                                         ; { field_name = "a"
                                           ; typ = TInt
                                           ; v = Apply
                                                   ( "addi"
                                                   , [Var "x"; Int 12]
                                                   )
                                           }
                                         ]
                                      ]
                                    )
                                )
                       }
                )
  in
  (target, root1, root2)

  let jit_case1 () =
    let target, root1, root2 = case1 in
    let rt = Runtime.create [target] in
    let open Ctypes in
    let ft = Foreign.funptr (Ctypes.int64_t @-> Ctypes.int64_t @-> (returning Ctypes.int64_t)) in
    let (t1, f1) = Runtime.bind rt root1 ft in
    let (t2, f2) = Runtime.bind rt root2 ft in
    (t1 +. t2, (f1, f2))

  let interpret_case1 () =
    let target, root1, root2 = case1 in
    let it = Interp.create [target; root1; root2] in
    let f1 x y =
      Interp.exec it (Apply ("root1", [Int x; Int y]))
    in
    let f2 x y =
      Interp.exec it (Apply ("root2", [Int x; Int y]))
    in
    f1, f2

  let ocaml_case1 () =
    let target r =
      r#x + r#y
    in

    let root1 x y =
      let xy = x + y in
      let o =
        object
          method x = x
          method y = y
          method xy = xy
        end
      in
      target o
    in

    let root2 x y =
      let xy = x + y in
      let a = x + 12 in
      let o =
        object
          method x = x
          method y = y
          method xy = xy
          method a = a
        end
      in
      target o
    in
    (root1, root2)

  let run count cases =
    let i64_bounds = Int64.of_int 1000000 in
    let int_bounds = 1000000 in

    let (t, (f1, f2)) = jit_case1 () in

    let jit_case _ =
      let x, y = Random.int64 i64_bounds, Random.int64 i64_bounds in
      let f = if Random.bool () then f1 else f2 in
      (f, x, y)
    in

    let f1, f2 = interpret_case1 () in
    let int_case _ =
      let x, y = Random.int int_bounds, Random.int int_bounds in
      let f = if Random.bool () then f1 else f2 in
      (f, x, y)
    in

    let f1, f2 = ocaml_case1 () in
    let ocaml_case _ =
      let x, y = Random.int int_bounds, Random.int int_bounds in
      let f = if Random.bool () then f1 else f2 in
      (f, x, y)
    in

    let jit_cases = Array.init cases jit_case in
    let int_cases = Array.init cases int_case in
    let ocaml_cases = Array.init cases ocaml_case in

    let rec bench_exec c start to_exec =
      if c < 0 then
        Unix.gettimeofday () -. start
      else
        begin
          let (f, x, y) = to_exec.(c mod cases) in
          let _ = f x y in
          bench_exec (c - 1) start to_exec
        end
    in

    let jit_elapsed = bench_exec count (Unix.gettimeofday ()) jit_cases in
    let int_elapsed = bench_exec count (Unix.gettimeofday ()) int_cases in
    let ocaml_elapsed = bench_exec count (Unix.gettimeofday ()) ocaml_cases in
    (t, jit_elapsed, int_elapsed, ocaml_elapsed)
end
