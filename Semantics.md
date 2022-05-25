# a86 Semantics

This file provides the semantics used in the a86 interpreter. It contains a lot
of plain-text code blocks because I didn't want to format everything fancy yet.


## Syntax

```text
reg  ::= rax | rbx | rcx | rdx
      |  rbp | rsp | rsi | rdi
      |  r8  | r9  | r10 | r11
      |  r11 | r12 | r13 | r14

r    ::= reg | eax

fs   ::= CF | ZF | SF | OF

l    ::= <labels>

z    ::= <integers>

bc   ::= <integers in (0, 63)>

off  ::= Offset r z

inst ::= Label l
      |  Ret
      |  Call (l | r)
      |  Mov  (r | off) (r | off | z)
      |  Add  r         (r | off | z)
      |  Sub  r         (r | off | z)
      |  Cmp  (r | off) (r | off | z)
      |  Jmp  (l | r)
      |  Je   (l | r)
      |  Jne  (l | r)
      |  Jl   (l | r)
      |  Jg   (l | r)
      |  And  (r | off) (r | off | z)
      |  Or   (r | off) (r | off | z)
      |  Xor  (r | off) (r | off | z)
      |  Sal  r         bc
      |  Sar  r         bc
      |  Push (z | r)
      |  Pop  r
      |  Lea  (r | off) l

prog ::= Program (inst ...+)
```


## Semantics (Informal)

We first set out an informal semantics given in natural English. This will then
be revised into a formal semantics in the next section.

First, we define a *program*. A program is a list of instructions, where:

  * The list must be non-empty.
  * The first instruction must be a `Label` and is used as the entry point
    of the program (i.e., it is where execution will begin).
  * Two `Label` instructions in the same program cannot use the same name.

All a86 programs also run with some limited amount of register-external memory
called the *stack*. The stack starts at the highest available address space and
"grows downwards", meaning adding something to the stack *decrements* the
pointer to the current position in the stack.

There are sixteen 64-bit registers, each corresponding to one of `reg`. The
special register reference `eax` refers to the lower 32 bits of the `rax`
register.

There are four single-bit registers, each corresponding to one of `fs`. These
registers are called *flags* and are used for arithmetic and comparison:

  * `OF` --- Overflow Flag
      * Set if *adding* two numbers with the same sign bit and the result has a
        different sign bit.
      * Set if *subtracting* a negative from a positive and the result is
        negative.
      * Set if *subtracting* a positive from a negative and the result is
        positive.
  * `SF` --- Sign Flag
      * Set to the value of the sign bit of the result.
  * `ZF` --- Zero Flag
      * Set if the result is `0`.
  * `CF` --- Carry Flag
      * Set if the (unsigned) arithmetic operation required an extra bit.

We say a flag is "set" if we store the value `1` in it, or "cleared" if we store
the value `0`. Sometimes we might say "set the value of <flag> to `0`", which is
an unfortunate overloading of a term but is at least unambiguous.

There are 20 instructions, which work as follows:

  * `Label l` creates a new label named `l` that points to the next instruction.
  * `Ret` pops an address from the stack and jumps to it.
  * `Call dst` pushes the return address onto the stack, then jumps to the
    address indicated by the label or register in `dst`.
  * `Mov dst src` moves the contents of/value at `src` into `dst`.
      * Either `dst` or `src` may be an offset, but not both.
  * `Add dst src` adds `src` to `dst` and writes the result to `dst`.
  * `Sub dst src` subtracts `src` from `dst` and writes the result to `dst.`
  * `Cmp a1 a2` compares `a1` to `a2` and sets the corresponding condition
    codes:
      * `CF = 1` if an extra bit was needed to complete the computation.
      * `ZF = 1` if `a2 - a1 == 0`.
      * `SF = 1` if `a2 - a1 <  0`.
      * `OF = 1` if either:
          * `a > 0 && b < 0 && (b - a) > 0`.
          * `a < 0 && b > 0 && (b - a) < 0`.
  * `Jmp dst` jumps to `dst`.
  * `Je dst` jumps to `dst` if `ZF` is set.
  * `Jne dst` jumps to `dst` if `ZF` is not set.
  * `Jl dst` jumps to `dst` if `SF` and `OF` have different values.
  * `Jg dst` jumps to `dst` if `SF` and `OF` are set to the same value and `ZF`
    is unset.
  * `And dst src` computes `dst & src` and stores the result in `dst`.
  * `Or dst src` computes `dst | src` and stores the result in `dst`.
  * `Xor dst src` computes `dst ^ src` and stores the result in `dst`.
  * `Sal dst i` arithmetically shifts the bits in `dst` to the left by `i` bits
    and stores the result in `dst`. The new bits from the right are `0`s, and
    the `CF` flag is updated to the value of the most-significant bit during
    each shift.
      * If `i` is `1`, the `OF` flag is set to `0` if the most-significant bit
        of the result is the same as the `CF` flag, otherwise it is set to `1`.
  * `Sar dst i` arithmetically shift the bits in `dst` to the right by `i` bits
    and stores the result in `dst`. The new bits from the left are duplicated
    from the original sign (most-significant) bit, and the `CF` flag is updated
    to the value of the least-significant bit during each shift.
      * The `OF` flag is always cleared.
  * `Push src` decrements the stack pointer and stores the `src` operand on the
    top of the stack.
  * `Pop dst` loads the value from the top of the stack into the `dst` operand
    and increments the stack pointer.
  * `Lea dst l` loads the address of the label `l` into `dst`.
