#lang racket

(provide emulator-result

         current-emulator-after-thunk
         current-emulator-on-exit-thunk
         current-emulator-on-raise-thunk
         current-emulator-exit-handler
         current-emulator-exception-handler
         current-emulator-non-exception-handler
         current-emulator-body-thunk

         normal-emulator-after-thunk
         normal-emulator-on-exit-thunk
         normal-emulator-on-raise-thunk
         normal-emulator-exit-handler
         normal-emulator-exception-handler
         normal-emulator-non-exception-handler
         normal-emulator-body-thunk

         io-emulator-after-thunk
         io-emulator-on-exit-thunk
         io-emulator-on-raise-thunk
         io-emulator-exit-handler
         io-emulator-exception-handler
         io-emulator-non-exception-handler
         io-emulator-body-thunk

         run-emulator*
         run-emulator
         run-emulator/io)

(require "emulator.rkt"
         "runtime.rkt"
         "stacktrace.rkt"
         "../utility.rkt")

(define emulator-result                        (make-parameter #f))

(define current-emulator-after-thunk           (make-parameter #f))
(define current-emulator-on-exit-thunk         (make-parameter #f))
(define current-emulator-on-raise-thunk        (make-parameter #f))
(define current-emulator-exit-handler          (make-parameter #f))
(define current-emulator-exception-handler     (make-parameter #f))
(define current-emulator-non-exception-handler (make-parameter #f))
(define current-emulator-body-thunk            (make-parameter #f))

(define normal-emulator-after-thunk
  (λ ()
    (let ([r (emulator-result)])
      (if (a86-value? r)
          (a86-value->signed-integer r)
          r))))
(define io-emulator-after-thunk
  (λ ()
    (let ([r (emulator-result)]
          [p (current-runtime-output-port)])
      (cons (if (a86-value? r)
                (a86-value->signed-integer r)
                r)
            (if (string-port? p)
                (get-output-string p)
                p)))))

(define normal-emulator-on-exit-thunk void)
(define io-emulator-on-exit-thunk normal-emulator-on-exit-thunk)

(define normal-emulator-on-raise-thunk
  (λ () (current-emulator-after-thunk)))
(define io-emulator-on-raise-thunk
  (λ () (current-emulator-after-thunk)))

(define (normal-emulator-exit-handler who)
  (λ (v)
    (current-emulator-on-exit-thunk)
    (raise-user-error who
                      "program exited with status ~a"
                      v)))
(define io-emulator-exit-handler normal-emulator-exit-handler)

(define normal-emulator-exception-handler
  (λ (e)
    (raise-user-error
     (format "error encountered during evaluation:\n\n~a\nmachine stack trace:\n~a"
             (exn-message e)
             (format-stacktrace (current-emulator))))))
(define io-emulator-exception-handler normal-emulator-exception-handler)

(define normal-emulator-non-exception-handler
  (λ (e)
    (parameterize ([emulator-result e])
      (current-emulator-on-raise-thunk))))
(define io-emulator-non-exception-handler normal-emulator-exception-handler)

(define normal-emulator-body-thunk
  (λ ()
    (emulator-multi-step!)
    (parameterize ([emulator-result (emulator-register-ref 'rax)])
      (current-emulator-after-thunk))))
(define io-emulator-body-thunk normal-emulator-body-thunk)

(define (run-emulator* who instructions input-port output-port)
  (parameterize ([current-emulator (initialize-emulator instructions)]
                 [current-runtime-input-port   input-port]
                 [current-runtime-output-port output-port]
                 [exit-handler ((current-emulator-exit-handler) who)])
    (with-handlers ([exn?     (current-emulator-exception-handler)]
                    [(λ _ #t) (current-emulator-non-exception-handler)])
      ((current-emulator-body-thunk)))))

(define (run-emulator who instructions)
  (parameterize ([current-emulator-after-thunk           (or (current-emulator-after-thunk)
                                                             normal-emulator-after-thunk)]
                 [current-emulator-on-exit-thunk         (or (current-emulator-on-exit-thunk)
                                                             normal-emulator-on-exit-thunk)]
                 [current-emulator-on-raise-thunk        (or (current-emulator-on-raise-thunk)
                                                             normal-emulator-on-raise-thunk)]
                 [current-emulator-exit-handler          (or (current-emulator-exit-handler)
                                                             normal-emulator-exit-handler)]
                 [current-emulator-exception-handler     (or (current-emulator-exception-handler)
                                                             normal-emulator-exception-handler)]
                 [current-emulator-non-exception-handler (or (current-emulator-non-exception-handler)
                                                             normal-emulator-non-exception-handler)]
                 [current-emulator-body-thunk            (or (current-emulator-body-thunk)
                                                             normal-emulator-body-thunk)])
    (run-emulator* who instructions #f #f)))

(define (run-emulator/io who instructions input-port output-port)
  (parameterize ([current-emulator-after-thunk           (or (current-emulator-after-thunk)
                                                             io-emulator-after-thunk)]
                 [current-emulator-on-exit-thunk         (or (current-emulator-on-exit-thunk)
                                                             io-emulator-on-exit-thunk)]
                 [current-emulator-on-raise-thunk        (or (current-emulator-on-raise-thunk)
                                                             io-emulator-on-raise-thunk)]
                 [current-emulator-exit-handler          (or (current-emulator-exit-handler)
                                                             io-emulator-exit-handler)]
                 [current-emulator-exception-handler     (or (current-emulator-exception-handler)
                                                             io-emulator-exception-handler)]
                 [current-emulator-non-exception-handler (or (current-emulator-non-exception-handler)
                                                             io-emulator-non-exception-handler)]
                 [current-emulator-body-thunk            (or (current-emulator-body-thunk)
                                                             io-emulator-body-thunk)])
    (run-emulator* who instructions input-port output-port)))
