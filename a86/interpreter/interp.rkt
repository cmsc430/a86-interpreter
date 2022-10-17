#lang racket

(provide interp)

(require "state.rkt"
         "step.rkt")

(define (interp instructions
                #:entry-point [entry-point 'entry]
                #:runtime [runtime (hash)])
  (match (multi-step (initialize-state (define-program
                                         #:entry-point entry-point
                                         instructions)
                                       #:runtime runtime))
    [(cons state _)
     (hash-ref (State-registers state) 'rax)]))
