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
