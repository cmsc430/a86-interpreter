#lang racket

(provide current-emulator
         emulator-result
         asm-emulate
         asm-emulate/io)

(require "emulator.rkt"
         "runtime.rkt"
         "stacktrace.rkt")

(define emulator-result (make-parameter #f))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
(define (asm-emulate instructions
                     #:after   [after-thunk (λ () (emulator-result))]
                     #:on-exit [error-thunk (λ () (void))])
  (asm-emulate/io instructions #f #f #:after after-thunk #:on-exit error-thunk))

(define (asm-emulate/io instructions
                        input
                        [output-port (current-output-port)]
                        #:after   [after-thunk (λ () (emulator-result))]
                        #:on-exit [error-thunk (λ () (void))])
  (parameterize ([current-runtime-input-port input]
                 [current-runtime-output-port output-port]
                 [exit-handler (λ (v)
                                 (error-thunk)
                                 (raise-user-error 'asm-emulate/io
                                                   "program exited with status ~a"
                                                   v))]
                 [current-emulator (initialize-emulator instructions)])
    (with-handlers ([(λ _ #t)
                     (λ (e)
                       (raise-user-error
                        (format "error encountered during evaluation:\n\n~a\nmachine stack trace:\n~a"
                                (exn-message e)
                                (format-stacktrace (current-emulator)))))])
      (emulator-multi-step! (current-emulator)))
    (parameterize ([emulator-result (emulator-register-ref (current-emulator) 'rax)])
      (after-thunk))))
