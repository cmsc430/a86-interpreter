#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt")

(provide (all-defined-out))

;; TODO: comment
(struct State (labels registers flags memory) #:transparent)

;; TODO: comment
(define (initialize-state program)
  (let*-values
      ([(sp memory) (initialize-memory (Program-instructions program))]
       [(labels)
        (memory-fold (λ (labels addr value)
                       (if (Label? value)
                           (cons (cons addr (Label-x value)) labels)
                           labels))
                     (list)
                     memory
                     #f sp)]
       [(registers)
        (hash-set new-registers 'rsp sp)])
    (State labels registers new-flags memory)))

;; TODO: Implement this, then an [interp] function that calls it repeatedly.
(define (step state)
  state)
