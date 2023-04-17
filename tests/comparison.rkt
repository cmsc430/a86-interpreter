#lang racket/base

(require "utility.rkt"
         "../a86/ast.rkt")

(module+ test
  (require rackunit/text-ui))

(define (arith-test-maker op)
  (make-comparison-op-asm-maker
   (format "~a: ~~a and ~~a" (string-upcase (symbol->string (object-name op))))
   (op 'rax 'r8)))

(define arithmetic-tests
  (make-comparison-op-test-suites "arith-ops"
                                  (list Add Sub And Or Xor)
                                  arith-test-maker))

(module+ test
  (run-tests arithmetic-tests))

(define (jump-test-maker op)
  (make-comparison-op-asm-maker
   (format "~a after (Cmp ~~a ~~a)" (string-upcase (symbol->string (object-name op))))
   (let ([jump-target (gensym 'jump_target)])
     (seq (Cmp 'rax 'r8)
          (op jump-target)
          (Mov 'rax 10)
          (Ret)
          (Label jump-target)
          (Mov 'rax 11)))))

(define jump-tests
  (make-comparison-op-test-suites "jump-ops"
                                  (list Je Jne Jl Jle Jg Jge Jo Jno Jc Jnc)
                                  jump-test-maker))

(module+ test
  (run-tests jump-tests))

(define (cmov-test-maker op)
  (make-comparison-op-asm-maker
   (format "~a depending on (Cmp ~~a ~~a)" (string-upcase (symbol->string (object-name op))))
   (seq (Cmp 'rax 'r8)
        (Mov 'r9 10)
        (op 'rax 'r9))))

(define cmov-tests
  (make-comparison-op-test-suites "cmov-ops"
                                  (list Cmove Cmovne Cmovl Cmovle Cmovg Cmovge
                                        Cmovo Cmovno Cmovc Cmovnc)
                                  cmov-test-maker))

(module+ test
  (run-tests cmov-tests))
