#lang racket

(provide current-emulator
         emulator-result
         asm-emulate
         asm-emulate/io)

(require "emulator.rkt"
         "runtime.rkt"
         "stacktrace.rkt"
         "../utility.rkt")

(define emulator-result (make-parameter #f))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
(define (asm-emulate instructions
                     #:after [after-thunk (λ ()
                                            (let ([r (emulator-result)])
                                              (if (a86-value? r)
                                                  (a86-value->signed-integer r)
                                                  r)))]
                     #:on-exit [exit-thunk (λ () (void))]
                     #:on-raise [raise-thunk after-thunk])
  (asm-emulate/io instructions #f #f #:after after-thunk #:on-exit exit-thunk #:on-raise raise-thunk))

(define (asm-emulate/io instructions
                        input-port
                        [output-port (open-output-string)]
                        #:after [after-thunk (λ ()
                                               (let ([r (emulator-result)]
                                                     [p (current-runtime-output-port)])
                                                 (cons (if (a86-value? r)
                                                           (a86-value->signed-integer r)
                                                           r)
                                                       (if (string-port? p)
                                                           (get-output-string p)
                                                           p))))]
                        #:on-exit [exit-thunk (λ () (void))]
                        #:on-raise [raise-thunk after-thunk])
  (parameterize ([current-runtime-input-port input-port]
                 [current-runtime-output-port output-port]
                 [exit-handler (λ (v)
                                 (exit-thunk)
                                 (raise-user-error 'asm-emulate/io
                                                   "program exited with status ~a"
                                                   v))]
                 [current-emulator (initialize-emulator instructions)])
    (with-handlers ([exn?
                     (λ (e)
                       (raise-user-error
                        (format "error encountered during evaluation:\n\n~a\nmachine stack trace:\n~a"
                                (exn-message e)
                                (format-stacktrace (current-emulator)))))]
                    [(λ _ #t)
                     (λ (e)
                       (parameterize ([emulator-result e])
                         (raise-thunk)))])
      (emulator-multi-step! (current-emulator))
      (parameterize ([emulator-result (emulator-register-ref (current-emulator) 'rax)])
        (after-thunk)))))
