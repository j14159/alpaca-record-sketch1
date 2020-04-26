let report name f count cases =
  let (jt, jit, interp, ocaml) = f count cases in
  Printf.printf "==========\n%s\n==========\n" name;
  Printf.printf "JIT compilation time:   %f\n" jt;
  Printf.printf "JIT compiled run time:  %f\n" jit;
  Printf.printf "Interpreter run time:   %f\n" interp;
  Printf.printf "OCaml native run time:  %f\n" ocaml

let _ =
  Random.init 1234;
  let count = 1000000 in
  let cases = 1000 in

  report "Record add" Record_add_case.Case.run count cases;
  report "Addition" Addition.Case.run count cases
