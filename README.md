Alpaca Record Sketch #1
=====
[Alpaca](http://alpaca-lang.org) has row-polymorphic records, but the Erlang VM doesn't really provide any support for this beyond maps.  This repository is a small experiment to explore JIT compilation of simple operations on polymorphic records, attempting to be something in the vein of Christopher Alexander's "constructive diagrams" (see [Notes on the Synthesis of Form](https://www.goodreads.com/fa/book/show/320553.Notes_on_the_Synthesis_of_Form).  I have tried to keep this quite simple/restricted, including only very simple operations:

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

"Heavy-weight" because evaluating any function call will specialize/JIT the entire call graph contained within it ("eager" rather than lazy).

I'm using MCJIT because the OCaml LLVM bindings for 9.0.x seem to focus on that for now.

## Problems
There are a number of issues with the approach I've taken here, not least of which is that the way records are "compiled" right now won't work correctly with things like mailboxes used for concurrent message passing, even if they're limited to a specific record type.  I'm thinking of follow-up experiments ("sketches") to explore this more and they'll probably rely on something like [Self](https://selflanguage.org/)'s concept of "maps" and/or linked mailboxes that handle widening records over time.

## Original Definition
From my notebook:

> Limit to record creation and field access.  Polymorphic functions to extract fields, perform some operations (arithmetic), and construct new records.  JIT vs simple OCaml-based interpreter.
> 
> When a test harness generates a range of record types, can a simple JIT adapt, specialize call graphs, and outperform a basic interpreter?  By how much?
