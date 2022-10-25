#lang racket

(provide/contract
 [current-runtime (parameter/c (hash/c symbol? procedure?))]
 [asm-emulate     (-> (listof instruction?) any/c)])

(require "ast.rkt"
         "state.rkt"
         "step.rkt")

;; Functions needed at runtime are provided in this parameter.
(define current-runtime
  (make-parameter (hash)))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
;; Assume: entry point is "entry"
(define (asm-emulate instructions)
  (match (multi-step (initialize-state (define-program instructions)
                                       #:runtime (current-runtime)))
    [(cons state _)
     (hash-ref (State-registers state) 'rax)]))
