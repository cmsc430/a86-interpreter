#lang racket

(provide current-tui-state
         current-emulator
         current-state-index
         current-emulator-state
         current-instruction-pointer
         current-memory
         current-flags
         current-registers
         current-labels->addresses
         current-flag-ref
         current-register-ref
         current-memory-ref
         current-transactions
         previous-emulator-state
         previous-instruction-pointer
         previous-flags
         previous-registers
         previous-flag-ref
         previous-register-ref
         previous-memory-ref
         previous-transactions
         step!
         step-back!

         with-state-from-instructions
         with-state-from-input
         with-state-from-string
         with-state-from-file

         with-example-state)

(require "../../ast.rkt"
         "../../exn.rkt"
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

(define (current-flags)
  (emulator->flags (current-emulator)))

(define (current-registers)
  (emulator->registers (current-emulator)))

(define (current-labels->addresses)
  (Emulator-labels->addresses (current-emulator)))

(define (current-flag-ref flag)
  (emulator-flag-ref (current-emulator) flag))

(define (current-register-ref register)
  (emulator-register-ref (current-emulator) register))

(define (current-memory-ref address)
  (emulator-memory-ref (current-emulator) address))

(define (current-transactions)
  (StepState-memory-transactions (current-emulator-state)))

(define (previous-emulator-state)
  (emulator-state (current-emulator) -1))

(define (previous-instruction-pointer)
  (with-handlers ([exn:fail:a86? (λ (_) #f)])
    (StepState-ip (previous-emulator-state))))

(define (previous-flags)
  (with-handlers ([exn:fail:a86? (λ (_) #f)])
    (emulator->flags (current-emulator) -1)))

(define (previous-registers)
  (with-handlers ([exn:fail:a86? (λ (_) #f)])
    (emulator->registers (current-emulator) -1)))

(define (previous-flag-ref flag)
  (with-handlers ([exn:fail:a86? (λ (_) 'no-result)])
    (emulator-flag-ref (current-emulator) -1 flag)))

(define (previous-register-ref register)
  (with-handlers ([exn:fail:a86? (λ (_) #f)])
    (emulator-register-ref (current-emulator) -1 register)))

(define (previous-memory-ref address)
  (emulator-memory-ref (current-emulator) -1 address))

(define (previous-transactions)
  (StepState-memory-transactions (previous-emulator-state)))

(define (step!)
  (emulator-step! (current-emulator)))

(define (step-back!)
  (emulator-step-back! (current-emulator)))

(define-syntax (with-state-from-instructions stx)
  (syntax-parse stx
    [(_ instructions runtime body ...+)
     #'(begin (displayln (format "using runtime: ~v" runtime))
              (parameterize ([current-runtime runtime])
                (let ([emulator (initialize-emulator instructions)])
                  (parameterize ([current-tui-state (tui-state emulator)])
                    body ...))))]))

(define-syntax (with-state-from-input stx)
  (syntax-parse stx
    [(_ runtime body ...+)
     #'(with-state-from-instructions (read-instructions) runtime body ...)]))

(define-syntax (with-state-from-string stx)
  (syntax-parse stx
    [(_ str runtime body ...+)
     #'(with-state-from-instructions
         (string->instructions str)
         runtime
         body ...)]))

(define-syntax (with-state-from-file stx)
  (syntax-parse stx
    [(_ path runtime body ...+)
     #'(with-state-from-string
         (file->string path)
         runtime
         body ...)]))

(define-syntax (with-example-state stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(parameterize ([current-runtime loot])
         (let ([emulator (initialize-emulator
                          #;(list (Extern 'entry)
                                (Label 'entry)
                                (Mov 'rax #xffffffffffffffff)
                                (Add 'rax 1)
                                (Ret))
                          #;(list
                           (Extern 'peek_byte)
                           (Extern 'read_byte)
                           (Extern 'write_byte)
                           (Extern 'raise_error)
                           (Global 'entry)
                           (Label 'entry)
                           (Push 'rbx)
                           (Push 'r15)
                           (Mov 'rbx 'rdi)
                           (Mov 'rax 272)
                           (Push 'rax)
                           (Mov 'rax 672)
                           (Push 'rax)
                           (Mov 'rax 152)
                           (Mov (Offset 'rbx 0) 'rax)
                           (Pop 'rax)
                           (Mov (Offset 'rbx 8) 'rax)
                           (Mov 'rax 'rbx)
                           (Or 'rax 2)
                           (Add 'rbx 16)
                           (Mov (Offset 'rbx 0) 'rax)
                           (Pop 'rax)
                           (Mov (Offset 'rbx 8) 'rax)
                           (Mov 'rax 'rbx)
                           (Or 'rax 2)
                           (Add 'rbx 16)
                           (Pop 'r15)
                           (Pop 'rbx)
                           (Ret)
                           (Label 'raise_error_align)
                           (Or 'rsp 8)
                           (Jmp 'raise_error))
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
