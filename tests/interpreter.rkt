#lang racket/base

(require "utility.rkt"
         "../interpreter/registers.rkt"
         "../interpreter/utility.rkt"
         rackunit)

(define all-register-names (cons 'eax register-names))
(define offset-values (list 0 4 8))
(define immediate-values (list 0 1 2 4 -1 (max-signed) (max-unsigned)))

(define/provide-test-suite
  instruction-creation-tests
  ;; Extern/Label
  (for ([register (append all-register-names
                          (list 1 2.0 "three" #f))])
    (test-exn (format "Extern with non-symbol name ~a" register)
              exn:fail?
              (λ () (Extern register)))
    (test-exn (format "Label with non-symbol name ~a" register)
              exn:fail?
              (λ () (Label register))))
  (test-not-exn "Extern with symbol name 'extern"
                (λ () (Extern 'extern)))
  (test-not-exn "Label with symbol name 'label"
                (λ () (Label 'label)))
  ;; Ret
  (test-not-exn "Ret"
                (λ () (Ret)))
  ;; Call
  (test-not-exn "Call function label"
                (λ () (Call 'function)))
  (for ([register all-register-names])
    (test-not-exn "Call register"
                  (λ () (Call register))))
  ;; Mov
  (for ([dst-register all-register-names])
    (for ([src-register all-register-names])
      (test-not-exn (format "Mov to register ~a from register ~a" dst-register src-register)
                    (λ () (Mov dst-register src-register)))
      (for ([i offset-values])
        (test-not-exn (format "Mov to register ~a from register ~a with offset ~a" dst-register src-register i)
                      (λ () (Mov dst-register (offset src-register i))))))
    (for ([value immediate-values])
      (test-not-exn (format "Mov value ~a to register ~a" dst-register value)
                    (λ () (Mov dst-register value))))
    (test-exn "Mov to register ~a from non-register label 'label"
              exn:fail?
              (λ () (Mov dst-register 'label))))
  (for* ([dst-register all-register-names]
         [i offset-values])
    (let ([os (offset dst-register i)])
      (for ([src-register all-register-names])
        (test-not-exn (format "Mov to register ~a with offset ~a from register ~a" dst-register i src-register)
                      (λ () (Mov os src-register)))
        (for ([j offset-values])
          (test-exn (format "Mov to register ~a with offset ~a from register ~a with offset ~a"
                            dst-register i
                            src-register j)
                    exn:fail?
                    (λ ()  (Mov os (offset src-register j)))))
        (for ([value immediate-values])
          (test-exn (format "Mov value ~a to register ~a with offset ~a" value dst-register i)
                    exn:fail?
                    (λ () (Mov os value)))))))
  ;; Add
  (for ([dst-register all-register-names])
    (for ([src-register all-register-names])
      (test-not-exn (format "Add register ~a to register ~a" src-register dst-register)
                    (λ () (Add dst-register src-register)))
      (for ([i offset-values])
        (test-not-exn (format "Add value from register ~a with offset ~a to register ~a" src-register i dst-register)
                      (λ () (Add dst-register (offset src-register i))))
        (test-exn (format "Add register ~a to register ~a with offset ~a" src-register dst-register i)
                  exn:fail?
                  (λ () (Add (offset dst-register i) src-register)))))
    (for ([value immediate-values])
      (test-not-exn (format "Add value ~a to register ~a" value dst-register)
                    (λ () (Add dst-register value)))
      (test-exn (format "Add register ~a to value ~a" dst-register value)
                exn:fail?
                (λ () (Add value dst-register)))))
  ;; Sub
  (for ([dst-register all-register-names])
    (for ([src-register all-register-names])
      (test-not-exn (format "Sub register ~a to register ~a" src-register dst-register)
                    (λ () (Sub dst-register src-register)))
      (for ([i offset-values])
        (test-not-exn (format "Sub value from register ~a with offset ~a to register ~a" src-register i dst-register)
                      (λ () (Sub dst-register (offset src-register i))))
        (test-exn (format "Sub register ~a to register ~a with offset ~a" src-register dst-register i)
                  exn:fail?
                  (λ () (Sub (offset dst-register i) src-register)))))
    (for ([value immediate-values])
      (test-not-exn (format "Sub value ~a to register ~a" value dst-register)
                    (λ () (Sub dst-register value)))
      (test-exn (format "Sub register ~a to value ~a" dst-register value)
                exn:fail?
                (λ () (Sub value dst-register)))))
  ;; Cmp
  ;; Jmp
  ;; Je
  ;; Jne
  ;; Jl
  ;; Jg
  ;; And
  ;; Or
  ;; Xor
  ;; Sal
  ;; Sar
  ;; Push
  ;; Pop
  ;; Lea
  )

(define/provide-test-suite
  program-creation-tests
  (test-program-exn "no Externs or Labels"
                    exn:fail:user?
                    (λ () (Program (list (Mov 'rax 1)))))
  (test-program-exn "Extern but no Label"
                    exn:fail:user?
                    (λ () (Program (list (Extern 'extern)
                                         (Mov 'rax 1)))))
  (test-program-exn "Extern after Label"
                    exn:fail:user?
                    (λ () (Program (list (Label 'label)
                                         (Extern 'extern)))))
  (test-program "program with only one Label"
                (Program (list (Label 'entry))))
  (test-program "program with Extern and Label"
                (Program (list (Extern 'extern)
                               (Label 'entry)))
                (hash 'extern (λ () (error "don't run this"))))
  (test-program "program with Mov"
                (Program (list (Label 'entry)
                               (Mov 'rax 1)))
                #:with-registers (hash 'rax 1))
  (test-program "program with Add"
                (Program (list (Label 'entry)
                               (Mov 'rax 1)
                               (Add 'rax 'rax)))
                #:with-registers (hash 'rax 2))
  (test-program "program with Add and ZF"
                (Program (list (Label 'entry)
                               (Mov 'rax 1)
                               (Add 'rax -1)))
                #:with-flags (hash 'ZF #t)
                #:with-registers (hash 'rax 0)))

(define (make-collatz-program n)
  (Program (list (Label 'entry)
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
                 (Label 'finish))))

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

(define/provide-test-suite
  simple-execution-tests
  (for ([n (in-range 1 20)])
    (let ([steps (collatz n)]
          [prog (make-collatz-program n)])
      (test-program (format "Collatz ~a" n)
                    prog
                    #:with-registers (hash 'rax 1
                                           'rbx steps)))))

(define (make-flag-test-program op lhs rhs)
  (Program (list (Label 'entry)
                 (Mov 'rax lhs)
                 (op 'rax rhs))))

(define/provide-test-suite
  addition-flag-setting-tests
  (test-program "add two small numbers to positive sum"
                (make-flag-test-program Add 1 1)
                #:with-flags (make-new-flags)
                #:with-registers (hash 'rax 2))
  (test-program "add two small numbers to negative sum"
                (make-flag-test-program Add -2 1)
                #:with-flags (make-new-flags #:sign #t)
                #:with-registers (hash 'rax -1))
  (test-program "add least signed value to greatest signed value"
                (make-flag-test-program Add (max-signed) (min-signed))
                #:with-flags (make-new-flags #:sign #t)
                #:with-registers (hash 'rax (max-unsigned)))
  (test-program "add two zeroes"
                (make-flag-test-program Add 0 0)
                #:with-flags (make-new-flags #:zero #t)
                #:with-registers (hash 'rax 0))
  (test-program "add 1100... to greatest signed value"
                (make-flag-test-program Add (max-signed) (make-full-mask -2))
                #:with-flags (make-new-flags #:carry #t)
                #:with-registers (hash 'rax (make-full-mask (- (word-size-bits) 2))))
  (test-program "add two small numbers to zero sum"
                (make-flag-test-program Add -1 1)
                #:with-flags (make-new-flags #:zero #t #:carry #t)
                #:with-registers (hash 'rax 0))
  (test-program "add one to greatest unsigned value"
                (make-flag-test-program Add (max-unsigned) 1)
                #:with-flags (make-new-flags #:zero #t #:carry #t)
                #:with-registers (hash 'rax 0))
  (test-program "add one to greatest signed value"
                (make-flag-test-program Add (max-signed) 1)
                #:with-flags (make-new-flags #:overflow #t #:sign #t)
                #:with-registers (hash 'rax (min-signed)))
  (test-program "add least signed value to greatest unsigned value"
                (make-flag-test-program Add (max-unsigned) (min-signed))
                #:with-flags (make-new-flags #:overflow #t #:carry #t)
                #:with-registers (hash 'rax (max-signed)))
  (test-program "add 10...1 to least signed value"
                (make-flag-test-program Add (max-signed) (max-signed))
                #:with-flags (make-new-flags #:overflow #t #:sign #t)
                #:with-registers (hash 'rax (sub1 (max-unsigned))))
  (test-program "add 1100... to 1100...1"
                (make-flag-test-program Add (make-full-mask -2) (add1 (make-full-mask -2)))
                #:with-flags (make-new-flags #:sign #t #:carry #t)
                #:with-registers (hash 'rax (add1 (min-signed))))
  (test-program "add least signed value to least signed value"
                (make-flag-test-program Add (min-signed) (min-signed))
                #:with-flags (make-new-flags #:overflow #t #:zero #t #:carry #t)
                #:with-registers (hash 'rax 0)))

(define/provide-test-suite
  subtraction-flag-setting-tests
  (test-program "subtract two small numbers to positive difference"
                (make-flag-test-program Sub 2 1)
                #:with-flags (make-new-flags)
                #:with-registers (hash 'rax 1))
  (test-program "subtract greatest signed value from least signed value"
                (make-flag-test-program Sub (min-signed) (max-signed))
                #:with-flags (make-new-flags #:overflow #t)
                #:with-registers (hash 'rax 1))
  (test-program "subtract 1 from least signed value"
                (make-flag-test-program Sub (min-signed) 1)
                #:with-flags (make-new-flags #:overflow #t)
                #:with-registers (hash 'rax (make-full-mask (sub1 (word-size-bits)))))
  (test-program "subtract two zeroes"
                (make-flag-test-program Sub 0 0)
                #:with-flags (make-new-flags #:zero #t)
                #:with-registers (hash 'rax 0))
  (test-program "subtract greatest unsigned value from least unsigned value"
                (make-flag-test-program Sub (min-unsigned) (max-unsigned))
                #:with-flags (make-new-flags #:carry #t)
                #:with-registers (hash 'rax 1))
  (test-program "subtract 1 from 0"
                (make-flag-test-program Sub 0 1)
                #:with-flags (make-new-flags #:sign #t #:carry #t)
                #:with-registers (hash 'rax (max-unsigned)))
  (test-program "subtract two small numbers to negative difference"
                (make-flag-test-program Sub 1 2)
                #:with-flags (make-new-flags #:sign #t #:carry #t)
                #:with-registers (hash 'rax -1))
  (test-program "subtract least signed value from greatest signed value"
                (make-flag-test-program Sub (max-signed) (min-signed))
                #:with-flags (make-new-flags #:overflow #t #:sign #t #:carry #t)
                #:with-registers (hash 'rax (max-unsigned))))
