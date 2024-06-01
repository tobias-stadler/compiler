This repository contains my work-in-progress optimizing compiler framework.

Main components:
- flexible IR that supports SSA-form for the middle-end and register-form for the back-end
- Pass-based framework for optimization and lowering
- custom DSL for IR rewriting and backend specification
- C language frontend
- RISC-V backend
# Architecture
## IR
A Function is contains as an intrusive linked-list of basic blocks, which contains an intrusive linked-list of instructions.
Each instruction contains an opcode and an array of operands.
Operands can be of many different types.
The most important are:
- SSADef: inline SSA value definition with an SSAType (e.g i32)
- ExternSSADef: SSA definition outside of an instruction (e.g. functions, basic blocks, stack slots)
- SSAUse: reference to an SSA definition
- Reg: virtual or physical register
- MInt: arbitrary width (<=64 bits) two's complement integer

SSA and register operands maintain def-use chains that allow for look-up of all related definition and use operands.
## C frontend
Lexer:
- hand-written two-layer state-machine
Preprocessor:
- fully "pull-based": tokens are preprocessed only when demanded by the parser
Parser:
- hand-written recursive descent parser 
- operator precedence parsing for expressions
- fully "LL(1)": only requires Lexer support for next() and peek()
- no backtracking
- some integrated semantic analysis: declarations are directly parsed into C types, the symbol table is populated
- error recovery features: dump source location and parser recursion context
IR Generation:
- lazy SSA construction using approach by M. Braun et al.

The goal is to fully implement C99. The Lexer, Preprocessor, Parser and AST already support nearly all language constructs (see TODO section).
IR generation currently only supports integer expressions, structured control flow, function calls with register-sized datatypes, basic structs and unions.
## RISC-V backend
- instruction selector currently supports RV32I
- emits textual GNU assembly
## DSL
`tools/dsl/` contains a compiler for a custom declarative DSL that is used for architecture specification, IR pattern matching and rewriting.
DSL code is compiled to C++ code.
### IR pattern rewriting
The DSL can be used to match an IR fragment and rewrite it to another IR fragment.
Matching is performed in a greedy fashion, larger patterns are tried first.
DSL IR patterns closely resemble the textual debug representation of the IR.

Example pattern used during instruction selection:
```
ir_pat {
  match {
    CONST_INT def(%c,i32) MInt(32,0);
    CMP def(%cmp,i32) eq %lhs %c;
  }
  emit {
    riscv::SLTIU def(%cmp,i32) %lhs imm32(1);
  }
}
```
More complicated behaviour can be specified using the built-in templating facilities and inline C++ code.
## Instruction Selection
Instruction selection is performed using the rewriting system explained above. Multiple passes over the IR are performed and different sets of patterns are applied during each pass.
Main passes:
- InstrExpansionPass: canonicalize and legalize instructions to ensure that the instruction selection patterns will match later
- InstrCombinePass: apply peephole optimization patterns until a fixed-point is reached
- InstrSelectPass: apply the instruction selection patterns
## Register Allocation
The current register allocator is based on a Chaitin-Briggs-style iterated optimistic graph coloring approach.
Main passes:
- LiveFlowPass: calculate live-in and live-out registers of each block using dataflow analysis
- LiveGraphPass: construct register interference graph (triangular bitset + adjacency lists)
- ColoringRegAllocPass: find node elimination order based on degree and spill cost, assign registers, insert spill code
- RegRewritePass: rewrite virtual registers to the physical registers assigned by the allocator
Current implementation status:
- COPY instructions are used as hints for guided coloring
- Spill cost estimation based on the number of register uses and interference graph degree
- All spilling is global
Before register allocation, SSA form is deconstructed by first restoring conventional SSA-form (by splitting the live-ranges of all PHI defs and uses using COPYs) and then replacing the PHI by a virtual register with multiple definitions.
To facilitate liveness analysis, all other SSA defs are also lowered to virtual registers.
# Example
```c
int calc(int a, int b) {
  int res = 0;
  if (a > 0) {
    res = a + b;
  }
  return res;
}
```

Generated IR:
```
%0:
REF_PARAM def(%3,i32)
REF_PARAM def(%4,i32)
CONST_INT def(%5,i32) MInt(32,0)
CONST_INT def(%6,i32) MInt(32,0)
CMP def(%7,i1) gt %3 %6
BR_COND %7 %1 %2
%1:
ADD def(%8,i32) %3 %4
BR %2
%2:
PHI def(%9,i32) [%8 %1 %5 %0]
RET %9
```

After instruction selection:
```
%0:
COPY def(%3,i32) X10
COPY def(%4,i32) X11
COPY def(%5,i32) X0
COPY def(%6,i32) X0
BLT %6 %3 %1 %2
%1:
ADD def(%7,i32) %3 %4
JAL X0 %2
%2:
PHI def(%8,i32) [%7 %1 %5 %0]
COPY def(X10) %8
JALR def(X0) X1 imm32(0) X10
```

After register allocation and branch lowering:
```
%0:
ADDI def(X5) X0 imm32(0)
BLT X0 X10 %1
JAL X0 %2
%1:
ADD def(X5) X10 X11
%2:
ADDI def(X10) X5 imm32(0)
JALR def(X0) X1 imm32(0) X10
```
# TODO
## C Frontend
- Lexer:
    - bigraphs and trigraphs
- Preprocessor: 
    - `#if` expression evaluation (blocked on const-expression evaluation), `#pragma`, `#error`, `#line`
    - correctly disable already expanded macro names
    - token concatination and stringification
- Parser and AST:
    - support K&R function declaration syntax
    - fix array declarators
    - fix handling of string, integer and char literals
    - fix handling of function prototypes
- IR generation:
    - blocker: const-expression evaluation
    - ABI for aggregates
    - ...
## IR
- replace Instr allocator with bump pointer allocator
## Register Allocation
- optimized spilling: live-range splitting, rematerialization (e.g. constants)
- better spill cost heuristic (loop analysis)
- dedicated coalescing phase
## RISC-V
- PostRALowering: proper branch relaxation
- ABILowering: fix stack passing
- floating point
- assembler, elf encoder, ...
