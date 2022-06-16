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
(define instruction-list3
  (list (Label 'entry)
        (Mov 'rax 42)
        (Label 'compare)
        (Mov 'rbx 1)
        (Cmp 'rax 'rbx)
        (Je 'finish)
        (And 'rbx 'rax)
        (Cmp 'rbx 0)
        (Je 'divide)
        (Label 'increase)
        (Mov 'rbx 'rax)
        (Add 'rax 'rbx)
        (Add 'rax 'rbx)
        (Add 'rax 1)
        (Jmp 'compare)
        (Label 'divide)
        (Sar 'rax 1)
        (Jmp 'compare)
        (Label 'finish)))
