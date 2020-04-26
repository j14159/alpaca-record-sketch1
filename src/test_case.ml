module type Test_case = sig
  (* [run number_of_calls_to_make case_count]

     Return tuple is:
       - Time taken in JIT.
       - Time taken by JIT-compiled code.
       - Time taken by the naive interpreter.
       - Time taken by native ocaml code.
   *)
  val run : int -> int -> (float * float * float * float)
end
