#lang racket

(provide run test-specs)

(require "../compile.rkt"
         "../parse.rkt"

         "../../../../a86/emulate.rkt")

(define (run e)
  (asm-emulate (compile (parse e))))

(define test-specs
  '(["Abscond"   [ 7  7]
                 [-8 -8]]

    ["Blackmail" [9 (add1 (add1 7))]
                 [7 (add1 (sub1 7))]]

    ["Con"       [1 (if (zero? 0) 1 2)]
                 [2 (if (zero? 1) 1 2)]
                 [2 (if (zero? -7) 1 2)]
                 [2 (if (zero? 0)
                        (if (zero? 1) 1 2)
                        7)]
                 [7 (if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7)]]))

(module+ test
  (require "../../test-specs.rkt")

  (run-test-specs "Con" test-specs run))
