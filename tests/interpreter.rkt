#lang racket

(require "utility.rkt"
         "../a86/ast.rkt"
         "../a86/registers.rkt"
         "../a86/runtime.rkt"
         "../a86/utility.rkt"
         rackunit

         (for-syntax racket/syntax
                     syntax/parse))

(provide (all-defined-out))

(define all-register-names (cons 'eax register-names))
(define offset-values (list 0 4 8))
(define immediate-values (list 0 1 2 4 -1 max-signed max-unsigned))

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

(define/provide-test-suite
  simple-execution-tests
  (for ([n (in-range 1 20)])
    (let ([steps (collatz n)]
          [prog (make-collatz-program n)])
      (test-program (format "Collatz ~a" n)
                    prog
                    #:with-registers (hash 'rax 1
                                           'rbx steps)))))

(define (format-value v)
  (cond
    [(equal? v max-signed)   'MAX_SIGNED]
    [(equal? v min-signed)   'MIN_SIGNED]
    [(equal? v max-unsigned) 'MAX_UNSIGNED]
    [else                    v]))

(define (test-arith-op op lhs rhs #:delete-files [delete-files #t])
  (let ([entry (gensym 'entry)])
    (test-asm (format "~a: ~a and ~a"
                      (string-upcase (symbol->string (object-name op)))
                      (format-value lhs)
                      (format-value rhs))
              (prog (Global entry)
                    (Label entry)
                    (Mov 'rax lhs)
                    (Mov 'r8 rhs)
                    (op  'rax 'r8)
                    (Ret))
              #:entry-label entry
              #:check-flags '(SF OF CF ZF)
              #:check-registers '(rax)
              #:delete-files delete-files)))

(define (make-arith-op-tester op #:delete-files [delete-files #t])
  (λ (lhs rhs)
    (test-arith-op op lhs rhs #:delete-files delete-files)))

(define arith-test-values
  (list -2 -1 0 1 2
        max-signed min-signed
        max-unsigned min-unsigned
        (make-mask -2) (add1 (make-mask -2))))

(define arith-test-pairs
  (for*/list ([lhs arith-test-values]
              [rhs arith-test-values])
    (list lhs rhs)))

(define-syntax (define/provide-arithmetic-test-suite stx)
  (syntax-parse stx
    [(_ op)
     (with-syntax ([name (format-id #'op "~a-arith-tests"
                                    (string-downcase (symbol->string (syntax-e #'op))))])
       #'(define/provide-test-suite
           name
           (let ([test-func (make-arith-op-tester op)])
             (map (λ (args) (apply test-func args))
                  arith-test-pairs))))]))

(define/provide-arithmetic-test-suite Add)
(define/provide-arithmetic-test-suite Sub)
(define/provide-arithmetic-test-suite And)
(define/provide-arithmetic-test-suite Or)
(define/provide-arithmetic-test-suite Xor)

(define (test-jump-op op lhs rhs #:delete-files [delete-files #t])
  (let ([entry (gensym 'entry)]
        [jump-target (gensym 'jump_target)])
    (test-asm (format "~a after (Cmp ~a ~a)"
                      (string-upcase (symbol->string (object-name op)))
                      (format-value lhs)
                      (format-value rhs))
              (prog (Global entry)
                    (Label entry)
                    (Mov 'rax lhs)
                    (Mov 'r8 rhs)
                    (Cmp 'rax 'r8)
                    (op jump-target)
                    (Mov 'rax 0)
                    (Ret)
                    (Label jump-target)
                    (Mov 'rax 1)
                    (Ret))
              #:entry-label entry
              #:check-registers '(rax)
              #:delete-files delete-files)))

(define (make-jump-op-tester op #:delete-files [delete-files #t])
  (λ (lhs rhs)
    (test-jump-op op lhs rhs #:delete-files delete-file)))

(define-syntax (define/provide-jump-test-suite stx)
  (syntax-parse stx
    [(_ op)
     (with-syntax ([name (format-id #'op "~a-jump-tests"
                                    (string-downcase (symbol->string (syntax-e #'op))))])
       #'(define/provide-test-suite
           name
           (let ([test-func (make-jump-op-tester op)])
             (map (λ (args) (apply test-func args))
                  arith-test-pairs))))]))

(define/provide-jump-test-suite Je)
(define/provide-jump-test-suite Jne)
(define/provide-jump-test-suite Jl)
(define/provide-jump-test-suite Jle)
(define/provide-jump-test-suite Jg)
(define/provide-jump-test-suite Jge)
(define/provide-jump-test-suite Jo)
(define/provide-jump-test-suite Jno)
(define/provide-jump-test-suite Jc)
(define/provide-jump-test-suite Jnc)
