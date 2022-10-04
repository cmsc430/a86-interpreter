#lang racket

(provide interp)

(require "state.rkt"
         "step.rkt")

(define (interp instructions
                #:entry-point [entry-point 'entry]
                #:runtime [runtime (hash)])
  (match (interp (initialize-state (Program instructions)
                                   #:entry-point entry-point
                                   #:runtime runtime))
    [(cons state _)
     (hash-ref (State-registers state) 'rax)]))
