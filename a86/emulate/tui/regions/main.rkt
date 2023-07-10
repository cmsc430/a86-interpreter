#lang racket
#|
Region Notes
============

The current size specification system is cumbersome and doesn't adapt as well as
I'd hoped, so I'm just going to specify everything in absolute sizes for now and
revise the entire windowing system later.

These are the "windows" in the interface:

  * Header (?)
  * Instructions
  * Flags + Registers
  * Stack, Heap, + Other Memory
  * Footer

The layout is:

╔═══════════════════════════════════════════════════════════════════════════════╦╦═════════════════════════╗
║ Instructions (.text Section)                                                  ║║ Flags                   ║
║                                                                               ║║                         ║
║->                     [0x40000d0]  (Call 'entry)                              ║║     OF  0     SF  0     ║
║                       [0x40000c8]  (Ret)                                      ║║     ZF  0     CF  0     ║
║    entry              [0x40000c0]  (Push 'rbx)                                ║║                         ║
║                       [0x40000b8]  (Push 'r15)                                ║║ Registers               ║
║                       [0x40000b0]  (Mov 'rbx 'rdi)                            ║║                         ║
║                       [0x40000a8]  (Mov 'rax 272)                             ║║rax    0x----------------║
║                       [0x40000a0]  (Push 'rax)                                ║║rbx    0x----------------║
║                       [0x4000098]  (Mov 'rax 672)                             ║║rcx    0x----------------║
║                       [0x4000090]  (Push 'rax)                                ║║rdx    0x----------------║
║                       [0x4000088]  (Mov 'rax 152)                             ║║rbp    0x----------------║
║                       [0x4000080]  (Mov (Offset 'rbx 0) 'rax)                 ║║rsp    0x----------------║
║                       [0x4000078]  (Pop 'rax)                                 ║║rsi    0x----------------║
║                       [0x4000070]  (Mov (Offset 'rbx 8) 'rax)                 ║║rdi    0x----------------║
║                       [0x4000068]  (Mov 'rax 'rbx)                            ║║r8     0x----------------║
║                       [0x4000060]  (Or 'rax 2)                                ║║r9     0x----------------║
║                       [0x4000058]  (Add 'rbx 16)                              ║║r10    0x----------------║
║                       [0x4000050]  (Mov (Offset 'rbx 0) 'rax)                 ║║r11    0x----------------║
║                       [0x4000048]  (Pop 'rax)                                 ║║r12    0x----------------║
║                       [0x4000040]  (Mov (Offset 'rbx 8) 'rax)                 ║║r13    0x----------------║
║                       [0x4000038]  (Mov 'rax 'rbx)                            ║║r14    0x----------------║
║                       [0x4000030]  (Or 'rax 2)                                ║║r15    0x----------------║
║                       [0x4000028]  (Add 'rbx 16)                              ║║                         ║
║                       [0x4000020]  (Pop 'r15)                                 ║║                         ║
║                       [0x4000018]  (Pop 'rbx)                                 ║║                         ║
║                       [0x4000010]  (Ret)                                      ║║                         ║
║    raise_error_align  [0x4000008]  (Or 'rsp 8)                                ║║                         ║
║                       [0x4000000]  (Jmp 'raise_error)                         ║║                         ║
║                                                                               ║║                         ║
║                                                                               ║║                         ║
║                                                                               ║║                         ║
╠═══════════════════════════════════════════════════════════════════════════════╩╩═════════════════════════╣
╠══════════════════════════════════╦╦══════════════════════════════════╦╦══════════════════════════════════╣
║ Stack                            ║║ Heap                             ║║ Static Memory                    ║
║                                  ║║                                  ║║                                  ║
║[0x--------]    0x----------------║║[0x--------]    0x----------------║║[0x--------]    0x----------------║
║[0x--------]    0x----------------║║[0x--------]    0x----------------║║[0x--------]    0x----------------║
║[0x--------]    0x----------------║║[0x--------]    0x----------------║║[0x--------]    0x----------------║
║[0x--------]    0x----------------║║[0x--------]    0x----------------║║[0x--------]    0x----------------║
║[0x--------]    0x----------------║║[0x--------]    0x----------------║║[0x--------]    0x----------------║
╠══════════════════════════════════╩╩══════════════════════════════════╩╩══════════════════════════════════╣
╠══════════════════════════════════════════════════════════════════════════════════════════════════════════╣
║ Footer                                                                                                   ║
║                                                                                                          ║
║                                                                                                          ║
║                                                                                                          ║
║                                                                                                          ║
╚══════════════════════════════════════════════════════════════════════════════════════════════════════════╝

Dimensions are:

  * Header:                         N/A
  * Instructions:                 80 x 34
  * Flags + Registers:            27 x 34
  * Stack, Heap, + Other Memory: 107 x  9
  * Footer:                      107 x  7

Part of the Flags + Registers region could be used for meta information, like
the file being run, the current step, the number of writes to memory, etc.
|#

(provide (all-from-out "header.rkt"
                       "instructions.rkt"
                       "registers.rkt"
                       "memory.rkt"))

(require "header.rkt"
         "instructions.rkt"
         "registers.rkt"
         "memory.rkt")
