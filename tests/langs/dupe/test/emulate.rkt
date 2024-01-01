#lang racket

(provide run test-specs)

(require "../compile.rkt"
         "../parse.rkt"
         "../types.rkt"

         "../../../../a86/emulate.rkt")

(define (run e)
  (bits->value (asm-emulate (compile (parse e)))))

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
                        7)]]

    ["Dupe"      [#t #t]
                 [#f #f]
                 [1  (if #t 1 2)]
                 [2  (if #f 1 2)]
                 [1  (if  0 1 2)]
                 [3  (if #t 3 4)]
                 [4  (if #f 3 4)]
                 [3  (if  0 3 4)]
                 [#f (zero? 4)]
                 [#t (zero? 0)]]))

(module+ test
  (require "../../test-specs.rkt")

  (run-test-specs "Dupe" test-specs run))
