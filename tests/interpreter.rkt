#lang racket

(require "utility.rkt"
         "../a86/ast.rkt"
         "../a86/registers.rkt"
         "../a86/runtime.rkt"
         "../a86/utility.rkt"
         rackunit)

(provide (all-defined-out))

(module+ test
  (require rackunit/text-ui))

(define-test-suite program-creation-tests
  (test-instructions-exn "no Externs or Labels"
                         exn:fail?
                         (Mov 'rax 1)
                         (Ret))
  (test-instructions-exn "Extern but no label"
                         exn:fail?
                         (Extern 'extern)
                         (Mov 'rax 1)
                         (Ret))
  (test-instructions "program with only one Label"
                     (Label 'entry)
                     (Ret))
  (test-instructions "program with Extern and Label"
                     #:runtime (runtime (hash 'extern (λ () (error "don't run this"))))
                     (Extern 'extern)
                     (Label 'entry)
                     (Ret))
  (test-instructions "program with Mov"
                     #:with-registers (hash 'rax 1)
                     (Label 'entry)
                     (Mov 'rax 1)
                     (Ret))
  (test-instructions "program with Add"
                     #:with-registers (hash 'rax 2)
                     (Label 'entry)
                     (Mov 'rax 1)
                     (Add 'rax 'rax)
                     (Ret))
  (test-instructions "program with Add and ZF"
                     #:with-flags (hash 'ZF #t)
                     #:with-registers (hash 'rax 0)
                     (Label 'entry)
                     (Mov 'rax 1)
                     (Add 'rax -1)
                     (Ret)))

(module+ test
  (run-tests program-creation-tests))

(define (make-collatz-program n)
  (prog (Label 'entry)
        (Mov 'rax n)
        (Mov 'rbx 1)
        (Label 'compare)
        (Mov 'rcx 1)
        (Cmp 'rax 'rcx)
        (Je 'finish)
        (Add 'rbx 1)
        (And 'rcx 'rax)
        (Cmp 'rcx 0)
        (Je 'divide)
        (Label 'increase)
        (Mov 'rcx 'rax)
        (Add 'rax 'rcx)
        (Add 'rax 'rcx)
        (Add 'rax 1)
        (Jmp 'compare)
        (Label 'divide)
        (Sar 'rax 1)
        (Jmp 'compare)
        (Label 'finish)
        (Ret)))

(define (collatz n)
  (define (calc n steps)
    (cond
      [(<= n 0)
       (error 'collatz "invalid n: ~a" n)]
      [(= 1 n)
       steps]
      [(even? n)
       (calc (/ n 2) (add1 steps))]
      [(odd? n)
       (calc (add1 (* 3 n))
             (add1 steps))]))
  (calc n 1))

(define-test-suite collatz-execution-tests
  (for ([n (in-range 1 20)])
    (let ([steps (collatz n)]
          [prog (make-collatz-program n)])
      (test-program (format "Collatz ~a" n)
                    prog
                    #:with-registers (hash 'rax 1
                                           'rbx steps)))))

(module+ test
  (run-tests collatz-execution-tests))
