#lang racket

(provide current-emulator
         emulator-result
         asm-emulate)

(require "emulator.rkt"
         "stacktrace.rkt")

(define emulator-result (make-parameter #f))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
;; Assume: entry point is "entry"
(define (asm-emulate instructions
                     #:after [after-thunk (λ () (emulator-result))]
                     #:on-exit [error-thunk (λ () (void))])
  (parameterize ([exit-handler (λ (v)
                                 (error-thunk)
                                 (raise-user-error 'asm-emulate
                                                   "program exited with status ~a"
                                                   v))]
                 [current-emulator (initialize-emulator instructions)])
    (with-handlers ([(λ (_) #t)
                     (λ (exn)
                       (raise-user-error
                        (format "encountered error during evaluation:\n\n~a~nmachine stack trace:\n~a"
                                (exn-message exn)
                                (format-stacktrace (current-emulator)))))])
      (emulator-multi-step! (current-emulator)))
    (parameterize ([emulator-result (emulator-register-ref (current-emulator) 'rax)])
      (after-thunk))))

#;(define (asm-emulate/io instructions input)
  42)
