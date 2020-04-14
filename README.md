Alpaca Record Sketch #1
=====
[Alpaca](http://alpaca-lang.org) has row-polymorphic records, but the Erlang VM doesn't really provide any support for this beyond maps.  This repository is a small experiment to explore JIT compilation of simple operations on polymorphic records, including:

- Record creation
- Individual field retrieval

Excluded are:

- Record restriction (narrowing)
- Record extension (adding new record fields)

Currently the following items work in a heavy-weight LLVM JIT, restricted to un-nested records:

- Record creation
- Function definition
- Function calls
- Integer creation
- Record field retrieval

"Heavy-weight" because evaluating any function call will specialize/JIT the entire call graph contained within it.

I'm using MCJIT because the OCaml LLVM bindings for 9.0.x seem to focus on that for now.

## Original Definition
From my notebook:

> Limit to record creation and field access.  Polymorphic functions to extract fields, perform some operations (arithmetic), and construct new records.  JIT vs simple OCaml-based interpreter.
> 
> When a test harness generates a range of record types, can a simple JIT adapt, specialize call graphs, and outperform a basic interpreter?  By how much?
