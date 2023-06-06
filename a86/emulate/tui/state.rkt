#lang racket

(provide current-tui-state
         current-emulator
         current-state-index
         current-emulator-state
         current-instruction-pointer
         current-memory
         step!
         step-back!

         with-example-state)

(require "../../ast.rkt"
         "../emulator.rkt"
         "../runtime.rkt"
         "../state.rkt"
         (submod "../emulator.rkt" private)

         (for-syntax syntax/parse))

(struct tui-state
  (emulator))

(define current-tui-state (make-parameter #f))

(define (current-emulator)
  (tui-state-emulator (current-tui-state)))

(define (current-state-index)
  (Emulator-current-index (current-emulator)))

(define (current-emulator-state)
  (emulator-state (current-emulator)))

(define (current-instruction-pointer)
  (StepState-ip (current-emulator-state)))

(define (current-memory)
  (Emulator-memory (current-emulator)))

(define (step!)
  (emulator-step! (current-emulator)))

(define (step-back!)
  (emulator-step-back! (current-emulator)))

(define-syntax (with-example-state stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(parameterize ([current-runtime loot])
         (let ([emulator (initialize-emulator
                          (list
                           (Extern 'peek_byte)
                           (Extern 'read_byte)
                           (Extern 'write_byte)
                           (Extern 'raise_error)
                           (Global 'entry)
                           (Label 'entry)
                           (Push 'rbx)
                           (Mov 'rbx 'rdi)
                           (Add 'rbx 0)
                           (Lea 'rax 'label_lambda10359_4df0)
                           (Mov (Offset 'rbx 0) 'rax)
                           (Mov 'rax 'rbx)
                           (Or 'rax 5)
                           (Add 'rbx 8)
                           (Push 'rax)
                           (Mov 'rax 48)
                           (Push 'rax)
                           (Lea 'rax 'ret10360)
                           (Push 'rax)
                           (Mov 'rax (Offset 'rsp 16))
                           (Push 'rax)
                           (Mov 'rax 112)
                           (Push 'rax)
                           (Mov 'rax (Offset 'rsp 8))
                           (Mov 'r9 'rax)
                           (And 'r9 7)
                           (Cmp 'r9 5)
                           (Jne 'raise_error_align)
                           (Xor 'rax 5)
                           (Mov 'rax (Offset 'rax 0))
                           (Jmp 'rax)
                           (Label 'ret10360)
                           (Pop 'r8)
                           (Mov 'r9 'r8)
                           (And 'r9 15)
                           (Cmp 'r9 0)
                           (Jne 'raise_error_align)
                           (Mov 'r9 'rax)
                           (And 'r9 15)
                           (Cmp 'r9 0)
                           (Jne 'raise_error_align)
                           (Sub 'r8 'rax)
                           (Mov 'rax 'r8)
                           (Add 'rsp 8)
                           (Add 'rsp 0)
                           (Pop 'rbx)
                           (Ret)
                           (Label 'label_lambda10359_4df0)
                           (Mov 'rax (Offset 'rsp 8))
                           (Xor 'rax 5)
                           (Mov 'rax (Offset 'rsp 0))
                           (Push 'rax)
                           (Mov 'rax 672)
                           (Pop 'r8)
                           (Mov 'r9 'r8)
                           (And 'r9 15)
                           (Cmp 'r9 0)
                           (Jne 'raise_error_align)
                           (Mov 'r9 'rax)
                           (And 'r9 15)
                           (Cmp 'r9 0)
                           (Jne 'raise_error_align)
                           (Add 'rax 'r8)
                           (Add 'rsp 16)
                           (Ret)
                           (Label 'raise_error_align)
                           (Mov 'r15 'rsp)
                           (And 'r15 8)
                           (Sub 'rsp 'r15)
                           (Call 'raise_error)))])
           (parameterize ([current-tui-state (tui-state emulator)])
             body ...)))]))
