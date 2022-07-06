#lang racket

(require rackunit
         "../interpreter/interp.rkt")

(provide (all-defined-out))

(define instruction-list1
  (list (Label 'entry)
        (Mov 'rax 42)))
(define instruction-list2
  (list (Label 'entry)
        (Mov 'rax 42)
        (Add 'rax 3)
        (Sub 'rax 9)))

;; Collatz Conjecture.
(define (collatz n)
  (Program (list (Label 'entry)
                 (Mov 'rax n)
                 (Mov 'rbx 1)
                 (Label 'compare)
                 (Mov 'rcx 1)
                 (Cmp 'rax 'rcx)
                 (Je 'finish)
                 (Add 'rbx 1)
                 (And 'rcx 'rax)
                 (Cmp 'rcx 0)
                 (Je 'divide)
                 (Label 'increase)
                 (Mov 'rcx 'rax)
                 (Add 'rax 'rcx)
                 (Add 'rax 'rcx)
                 (Add 'rax 1)
                 (Jmp 'compare)
                 (Label 'divide)
                 (Sar 'rax 1)
                 (Jmp 'compare)
                 (Label 'finish))))

;; Runtimes.
(define (test-external-func1)
  (let* ([prog (Program (list (Extern 'extern)
                              (Label 'entry)
                              (Mov 'rax 42)))]
         [runtime (hash 'extern (λ (x) (+ x 2)))]
         [state (initialize-state prog runtime)])
    (interp state 0)))

(define (test-external-func2)
  (let* ([prog (Program (list (Extern 'extern)
                              (Label 'entry)
                              (Mov 'rdi 42)
                              (Call 'extern)))]
         [runtime (hash 'extern (λ (x) (+ x 2)))]
         [state (initialize-state prog runtime)])
    (interp state 4)))

(define (test-external-func3)
  (let* ([prog (Program (list (Extern 'extern)
                              (Label 'entry)
                              (Mov 'rdi 1)
                              (Mov 'rsi 2)
                              (Mov 'rdx 3)
                              (Mov 'rcx 4)
                              (Mov 'r8 5)
                              (Mov 'r9 6)
                              (Push 7)
                              (Push 8)
                              (Call 'extern)))]
         [runtime (hash 'extern (λ (a1 a2 a3 a4 a5 a6 a7 a8)
                                  (+ a1 a2 a3 a4 a5 a6 a7 a8)))]
         [state (initialize-state prog runtime)])
    (interp state -1)))

(define (test-read-byte)
  (let* ([prog (Program (list (Extern 'read-byte)
                              (Label 'entry)
                              (Call 'read-byte)))]
         [runtime (hash 'read-byte (λ () (read-byte)))]
         [state (initialize-state prog runtime)])
    (interp state -1)))

(define (test-write-byte)
  (let* ([prog (Program (list (Extern 'write-byte)
                              (Label 'entry)
                              (Mov 'rdi 97)
                              (Call 'write-byte)))]
         [runtime (hash 'write-byte (λ (b) (write-byte b)))]
         [state (initialize-state prog runtime)])
    (interp state -1)))
