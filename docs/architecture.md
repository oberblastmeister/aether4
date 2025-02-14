# Architecture

Currently the pipeline is `Source -> Ast -> Tir -> Lir -> Abs_x86 -> Flat_x86`

The compiler should preserve SSA form throughout until `Flat_x86`.

## Ast

The parsed representation of the source code.

## Tir

Intended to be the ir for the middle-end of the compiler.
Should contain many C0 specific features such as structs and arrays.

## Lir

A low-level ir that should not know much about C0 specific concepts.
There are no high-level types, and the only type is `u64`, so we are working with raw memory.

## Register Allocation

Uses a greedy graph coloring register allocator.

## Abs_x86

An abstract form of of x86 in SSA form. Most instructions have two source operands and one destination operand.

```
d <- s1 * s2
```

Uses infinite virtual registers.

Register allocation happens on this ir.

We must legalize these instructions to turn them into valid `Flat_x86` instructions. For example
```ocaml
[
  Mov { dst = Reg Mach_reg.scratch; src = src1 }
  ; Add { dst = Reg Mach_reg.scratch; src = src2 }
  ; Mov { dst; src = Reg Mach_reg.scratch }
]

A single add instruction in `Abs_x86` gets turned into three `Flat_x86` instructions.
```

## Flat_x86

This is the real x86 with actual instructions.