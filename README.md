Alpaca Record Sketch #1
=====
[Alpaca](http://alpaca-lang.org) has row-polymorphic records, but the Erlang VM doesn't really provide any support for this beyond maps.  This repository is a small experiment to explore JIT compilation of simple operations on polymorphic records, attempting to be something in the vein of Christopher Alexander's "constructive diagrams" (see [Notes on the Synthesis of Form](https://www.goodreads.com/fa/book/show/320553.Notes_on_the_Synthesis_of_Form).  I have tried to keep this quite simple/restricted, including only very basic operations:

- Record creation
- Individual field retrieval

Excluded are:

- Record restriction (narrowing)
- Record extension (adding new record fields)

Currently the following items work in a heavy-weight LLVM JIT:

- Record creation
- Function definition
- Function calls
- Integer creation
- Record field retrieval
- Integer addition

"Heavy-weight" because evaluating any function call will specialize/JIT the entire call graph contained within it ("eager" rather than lazy) and it attempts to perform no further optimizations on top of the basic LLVM ones.  Both the interpreter and the JIT compiler are quite naive.

I'm using MCJIT because the OCaml LLVM bindings for 9.0.x seem to focus on that for now.

I added some OCaml implementations of the two simple profiling cases to see how far off I am from a "real" (non-naive) implementation, partly to see how much I need to learn.  Current results for 1M iterations of each with 1000 pre-generated "test cases" (values to input to functions):

```
==========
Record add
==========
JIT compilation time:   0.008154
JIT compiled run time:  0.196439
Interpreter run time:   0.559239
OCaml native run time:  0.041865
==========
Addition
==========
JIT compilation time:   0.001395
JIT compiled run time:  0.128283
Interpreter run time:   0.182004
OCaml native run time:  0.010069
```

Timings are in seconds using `Unix.gettimeofday`, "JIT compilation time" is "how long did the program spend on making LLVM IR and compiling it".

This depends on early [ppx_test_match](https://github.com/j14159/ppx_test_match) work which I have not yet submitted to OPAM.  You will need to install that locally if you want to run any of the tests in this repository.

## Problems
There are a number of issues with the approach I've taken here, not least of which is that the way records are "compiled" right now won't work correctly with things like mailboxes used for concurrent message passing, even if they're limited to a specific record type.  I'm thinking of follow-up experiments ("sketches") to explore this more and they'll probably rely on something like [Self](https://selflanguage.org/)'s concept of "maps" and/or linked mailboxes that handle widening records over time.

There's also a ridiculous amount of reading and research I need to do into basic optimization techniques and looking at what other languages do.

## Original Definition
From my notebook:

> Limit to record creation and field access.  Polymorphic functions to extract fields, perform some operations (arithmetic), and construct new records.  JIT vs simple OCaml-based interpreter.
> 
> When a test harness generates a range of record types, can a simple JIT adapt, specialize call graphs, and outperform a basic interpreter?  By how much?
