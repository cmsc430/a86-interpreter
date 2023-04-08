#lang racket

(require "asm.rkt"
         "../a86/ast.rkt"
         "../a86/emulator.rkt"
         "../a86/memory.rkt"
         "../a86/program.rkt"
         "../a86/runtime.rkt"
         "../a86/step.rkt"
         "../a86/utility.rkt"
         rackunit

         (for-syntax syntax/parse))

(provide (all-from-out "../a86/step.rkt")
         check-program
         check-program-exn
         test-program
         test-program-exn
         test-instructions
         test-instructions-exn
         test-asm)

(define (check-program instructions
                       [runtime #f]
                       [steps   -1]
                       #:with-flags     [expected-flags     (hash)]
                       #:with-registers [expected-registers (hash)]
                       #:with-memory    [expected-memory    (hash)])
  (let ([emulator (initialize-emulator instructions)])
    (parameterize ([emulator-step-count steps]
                   [current-runtime (or runtime (current-runtime))])
      (emulator-multi-step! emulator)
      (for ([(flag expected-value) (in-hash expected-flags)])
        (check-equal? (emulator-flag-ref emulator flag)
                      expected-value
                      (if expected-value
                          (format "~a flag incorrectly not set" flag)
                          (format "~a flag incorrectly set" flag))))
      (for ([(register expected-value) (in-hash expected-registers)])
        (check-equal? (emulator-register-ref emulator register)
                      (truncate-integer/unsigned expected-value)
                      (format "~a register not equal?" register)))
      (for ([(address expected-value) (in-hash expected-memory)])
        (check-equal? (emulator-memory-ref emulator address)
                      expected-value
                      (format "value at address ~a not equal?"
                              (format-word address 'hex)))))))

(define (check-program-exn exn-predicate
                           program-thunk
                           [runtime #f]
                           [steps   -1])
  (check-exn exn-predicate
             (λ ()
               (let ([emulator (initialize-emulator (program-thunk))])
                 (parameterize ([emulator-step-count steps]
                                [current-runtime (or runtime (current-runtime))])
                   (emulator-multi-step! emulator))))))

(define (test-program name
                      program
                      [runtime #f]
                      [steps   -1]
                      #:with-flags     [expected-flags     (hash)]
                      #:with-registers [expected-registers (hash)]
                      #:with-memory    [expected-memory    (hash)])
  (test-case name (check-program program
                                 runtime
                                 steps
                                 #:with-flags expected-flags
                                 #:with-registers expected-registers
                                 #:with-memory expected-memory)))

(define (test-program-exn name
                          exn-predicate
                          program-thunk
                          [runtime #f]
                          [steps   -1])
  (test-case name (check-program-exn exn-predicate
                                     program-thunk
                                     runtime
                                     steps)))

(define (test-instructions name
                           #:runtime        [runtime            #f]
                           #:steps          [steps              -1]
                           #:with-flags     [expected-flags     (hash)]
                           #:with-registers [expected-registers (hash)]
                           #:with-memory    [expected-memory    (hash)]
                           . instructions)
  (test-program name (prog (seq instructions)) runtime steps
                #:with-flags     expected-flags
                #:with-registers expected-registers
                #:with-memory    expected-memory))

(define-syntax (test-instructions-exn stx)
  (syntax-parse stx
    [(_ name
        exn-pred
        (~optional (~seq #:runtime runtime) #:defaults ([runtime #'#f]))
        (~optional (~seq #:steps steps) #:defaults ([steps #'-1]))
        instructions ...)
     #'(test-program-exn name exn-pred (thunk (prog (seq instructions ...))) runtime steps)]))

(define (test-asm name
                  instructions
                  #:entry-label     [entry-label         #f]
                  #:check-flags     [check-flags     (list)]
                  #:check-registers [check-registers (list)]
                  #:delete-files    [delete-files        #t])
  (define test-description name)
  (define output-annotation name)
  (when (list? name)
    (set! test-description (car name))
    (set! output-annotation (string-join name "\n")))
  (test-case test-description
    (let-values ([(flags regs)
                  (run-asm-test output-annotation
                                (wrap-asm-test instructions entry-label)
                                #:delete-files delete-files)])
      (let ([expected-flags (for/hash ([flag check-flags])
                              (values flag (hash-ref flags flag)))]
            [expected-registers (for/hash ([reg check-registers])
                                  (values reg (hash-ref regs reg)))])
        (check-program instructions
                       #:with-flags expected-flags
                       #:with-registers expected-registers)))))
