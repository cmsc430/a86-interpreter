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
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (make-test-suite
    "Con tests"
    (for/list ([test-spec (in-list test-specs)])
      (match test-spec
        [(list (? string? lang-name) (list rs ps) ...)
         (make-test-suite lang-name
                          (map (λ (r p)
                                 (delay-test
                                  (test-equal? (format "[~v <== ~v]" r p)
                                               (run p)
                                               r)))
                               rs ps))])))))
