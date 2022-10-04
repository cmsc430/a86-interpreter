#lang racket

(require rackunit
         "../a86/interpreter/step.rkt"
         "../a86/interpreter/state.rkt"
         "../a86/interpreter/memory.rkt")

(provide (all-from-out "../a86/interpreter/step.rkt")
         check-program
         check-program-exn
         test-program
         test-program-exn)

(define (check-program program
                       [runtime (hash)]
                       [steps -1]
                       #:with-flags [expected-flags (hash)]
                       #:with-registers [expected-registers (hash)]
                       #:with-memory [expected-memory (hash)])
  (let ([state (initialize-state program runtime)])
    (match (car (interp state steps))
      [(State _ _ _ _ _ _ actual-flags actual-registers actual-memory)
       (for ([(flag expected-value) (in-hash expected-flags)])
         (check-equal? (hash-ref actual-flags flag) expected-value (format "~a flag not equal?" flag)))
       (for ([(register expected-value) (in-hash expected-registers)])
         (check-equal? (hash-ref actual-registers register)
                       (integer->unsigned expected-value)
                       (format "~a register not equal?" register)))
       (for ([(address expected-value) (in-hash expected-memory)])
         (check-equal? (memory-ref actual-memory address) expected-value
                       (format "value at address ~a not equal?" address)))])))

(define (check-program-exn exn-predicate
                           program-thunk
                           [runtime (hash)]
                           [steps -1])
  (check-exn
   exn-predicate
   (λ () (interp (initialize-state (program-thunk) runtime) steps))))

(define (test-program name
                      program
                      [runtime (hash)]
                      [steps -1]
                      #:with-flags [expected-flags (hash)]
                      #:with-registers [expected-registers (hash)]
                      #:with-memory [expected-memory (hash)])
  (test-case name (check-program program
                                 runtime
                                 steps
                                 #:with-flags expected-flags
                                 #:with-registers expected-registers
                                 #:with-memory expected-memory)))

(define (test-program-exn name
                          exn-predicate
                          program-thunk
                          [runtime (hash)]
                          [steps -1])
  (test-case name (check-program-exn exn-predicate
                                     program-thunk
                                     runtime
                                     steps)))
