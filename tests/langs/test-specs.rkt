#lang racket

(provide test-specs?
         run-test-specs)

(require rackunit
         rackunit/text-ui)

(define (test-specs? tss)
  (and (list? tss)
       (andmap test-spec? tss)))

(define (test-spec? ts)
  (match ts
    [(list (? string? _) (list _ _) ...) #t]
    [_ #f]))

(define (run-test-specs name test-specs run)
  (unless (string? name)
    (raise-user-error 'run-test-specs "expected string for name; got: ~v" name))
  (unless (test-specs? test-specs)
    (raise-user-error 'run-test-specs "expected test-specs; got: ~v" test-specs))
  (unless (procedure? run)
    (raise-user-error 'run-test-specs "expected procedure; got: ~v" run))
  (cond
    [(procedure-arity-includes? run 1)
     (run-tests
      (make-test-suite
       name
       (for/list ([test-spec (in-list test-specs)])
         (match test-spec
           [(list (? string? lang-name) (list rs ps) ...)
            (make-test-suite lang-name
                             (map (λ (r p)
                                    (delay-test
                                     (test-equal? (format "[~v <== ~v]" r p)
                                                  (run p)
                                                  r)))
                                  rs ps))]))))]
    [(procedure-arity-includes? run 2)
     (run-tests
      (make-test-suite
       name
       (for/list ([test-spec (in-list test-specs)])
         (match test-spec
           [(list (? string? lang-name) (list (list outs rs) (list ins ps)) ...)
            (make-test-suite lang-name
                             (map (λ (out r in p)
                                    (delay-test
                                     (test-equal? (format "[(~v ~v) <== (~v ~v)]" out r in p)
                                                  (run (open-input-string in) p)
                                                  (cons r out))))
                                  outs rs ins ps))]))))]
    [else
     (raise-user-error 'run-test-specs "expected procedure with arity 1 or 2; got arity ~v" (procedure-arity run))]))
