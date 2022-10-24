#lang racket

(provide interp)

(require "state.rkt"
         "step.rkt")

(define (interp instructions
                #:runtime [runtime (hash)])
  (match (multi-step (initialize-state (define-program instructions)
                                       #:runtime runtime))
    [(cons state _)
     (hash-ref (State-registers state) 'rax)]))
