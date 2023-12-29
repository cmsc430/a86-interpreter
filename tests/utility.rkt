#lang racket

(require "asm.rkt"
         "../a86/ast.rkt"
         "../a86/emulate/emulator.rkt"
         "../a86/emulate/runtime.rkt"
         "../a86/utility.rkt"
         rackunit

         (for-syntax syntax/parse))

(provide check-program
         check-program-exn
         test-program
         test-program-exn
         test-instructions
         test-instructions-exn
         test-asm

         format-value
         comparison-op-test-values
         comparison-op-test-pairs
         make-comparison-op-asm-maker
         make-comparison-op-test-suites)

;; TODO: Implement alternatives to [test-exn] and [test-not-exn] that attempt to
;; run the code on x86 and verify that something goes wrong or nothing goes
;; wrong, without checking the resulting values.

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

(define void-thunk (λ () (void)))

(define (test-asm name
                  instructions
                  #:entry-label     [entry-label             #f]
                  #:check-flags     [check-flags         (list)]
                  #:check-registers [check-registers     (list)]
                  #:delete-files    [delete-files            #t]
                  #:before          [before-thunk    void-thunk]
                  #:after           [after-thunk     void-thunk])
  (define test-description name)
  (define output-annotation name)
  (when (list? name)
    (set! test-description (car name))
    (set! output-annotation (string-join name "\n")))
  (delay-test
   (test-case test-description
     (around
      (before-thunk)
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
                         #:with-registers expected-registers)))
      (after-thunk)))))

(define (format-value v)
  (cond
    [(equal? v max-signed)   'MAX_SIGNED]
    [(equal? v min-signed)   'MIN_SIGNED]
    [(equal? v max-unsigned) 'MAX_UNSIGNED]
    [else                    v]))

(define comparison-op-test-values
  (list -2 -1 0 1 2
        max-signed min-signed
        max-unsigned min-unsigned
        (make-mask -2) (add1 (make-mask -2))))

(define comparison-op-test-pairs
  (for*/list ([lhs comparison-op-test-values]
              [rhs comparison-op-test-values])
    (list lhs rhs)))

(define (make-comparison-op-asm-maker fmt-string instrs)
  (λ (lhs rhs
          #:delete-files [delete-files         #t]
          #:before       [before-thunk void-thunk]
          #:after        [after-thunk  void-thunk])
    (let ([entry (gensym 'entry)])
      (test-asm (format fmt-string
                        (format-value lhs)
                        (format-value rhs))
                (prog (Global entry)
                      (Label entry)
                      (Mov 'rax lhs)
                      (Mov 'r8 rhs)
                      instrs                 ;; <-- Instructions inserted here.
                      (Ret))
                #:entry-label entry
                #:check-registers '(rax r8)
                #:delete-files delete-files
                #:before before-thunk
                #:after after-thunk))))

(define (make-comparison-op-test-suites group-name ops test-maker)
  (make-test-suite
   group-name
   (for/list ([op ops])
     (let ([op-name (object-name op)])
       (make-test-suite (format "~a-comparison-tests" op-name)
                        (let ([test-func (test-maker op)]
                              [total (length comparison-op-test-pairs)])
                          (for/list ([pair (in-list comparison-op-test-pairs)]
                                     [i    (in-naturals 1)])
                            (test-func (car pair) (cadr pair)
                                       #:before (λ () (display
                                                       (format "~a: ~a ~a/~a"
                                                               group-name
                                                               op-name
                                                               i
                                                               total)))
                                       #:after (λ () (display "\r")))))
                        #:after (λ () (begin (display (make-string 80 #\space))
                                             (display "\r"))))))))
