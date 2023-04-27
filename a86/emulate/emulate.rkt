#lang racket

(provide current-emulator
         emulator-result
         asm-emulate)

(require "emulator.rkt")

(define current-emulator (make-parameter #f))
(define emulator-result (make-parameter #f))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
;; Assume: entry point is "entry"
(define (asm-emulate instructions
                     #:after [after-thunk (λ () (emulator-result))]
                     #:on-error [error-thunk (λ () (void))])
  (parameterize ([exit-handler (λ (v)
                                 (error-thunk)
                                 (error 'exit "program exited with status: ~a" v))]
                 [current-emulator (initialize-emulator instructions)])
    (emulator-multi-step! (current-emulator))
    (parameterize ([emulator-result (emulator-register-ref (current-emulator) 'rax)])
      (after-thunk))))

#;(define (asm-emulate/io instructions input)
  42)
