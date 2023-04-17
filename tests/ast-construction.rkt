#lang racket/base

(require "utility.rkt"
         "../a86/ast.rkt"
         "../a86/registers.rkt"
         "../a86/utility.rkt"
         rackunit

         (only-in racket/list flatten range))

(provide (all-defined-out))

(module+ test
  (require rackunit/text-ui))

(define all-register-names (cons 'eax register-names))
(define offset-values (list 0 4 8))
(define immediate-values (list 0 1 2 4 -1 max-signed max-unsigned))
(define symbol-values (append all-instruction-names
                              (list 'foo 'entry (gensym 'entry))))
(define valid-shift-values (range 0 word-size-bits))
(define invalid-shift-values (list -1 word-size-bits))

(define instruction-subset-op-creation-tests
  (make-test-suite
   "instruction-subset-op-creation-tests"
   (for/list ([subset-op (list Label Global Extern)])
     (let ([op-name (object-name subset-op)])
       (make-test-suite
        (format "~a-creation-tests" op-name)
        (append
         (for/list ([register-name all-register-names])
           (delay-test (test-exn (format "~a with register name ~a"
                                         op-name register-name)
                                 exn:fail?
                                 (λ () (subset-op register-name)))))
         (for/list ([non-symbol (list 1 2.0 "three" #f)])
           (delay-test (test-exn (format "~a with non-symbol name ~a"
                                         op-name non-symbol)
                                 exn:fail?
                                 (λ () (subset-op non-symbol)))))
         (for/list ([symbol-value symbol-values])
           (delay-test (test-not-exn (format "~a with symbol value ~a"
                                             op-name symbol-value)
                                     (λ () (subset-op symbol-value)))))))))))

(module+ test
  (run-tests instruction-subset-op-creation-tests))

(define stack-maintenance-op-creation-tests
  (make-test-suite
   "stack-maintenance-op-creation-tests"
   (list
    ;; Push
    (make-test-suite
     "Push-creation-tests"
     (append
      (for/list ([register-name all-register-names])
        (delay-test (test-not-exn (format "Push with register name ~a"
                                          register-name)
                                  (λ () (Push register-name)))))
      (for/list ([v immediate-values])
        (delay-test (test-not-exn (format "Push with immediate value ~a"
                                          v)
                                  (λ () (Push v)))))))
    ;; Pop
    (make-test-suite
     "Pop-creation-tests"
     (append
      (for/list ([register-name all-register-names])
        (delay-test (test-not-exn (format "Pop with register name ~a"
                                          register-name)
                                  (λ () (Pop register-name))))))))))

(module+ test
  (run-tests stack-maintenance-op-creation-tests))

(define function-call-op-creation-tests
  (make-test-suite
   "function-call-op-creation-tests"
   (list (make-test-suite
          "Call-creation-tests"
          (flatten
           (list
            (for/list ([register-name all-register-names])
              (list (delay-test (test-not-exn (format "Call to register ~a"
                                                      register-name)
                                              (λ () (Call register-name))))
                    (for/list ([i offset-values])
                      (delay-test (test-not-exn (format "Call to register ~a with offset ~a"
                                                        register-name i)
                                                (λ () (Call (Offset register-name i))))))))
            (for/list ([target symbol-values])
              (delay-test (test-not-exn (format "Call to target label ~a"
                                                target)
                                        (λ () (Call target)))))))))))

(module+ test
  (run-tests function-call-op-creation-tests))

;; ;; TODO: (list Not '(register))
;; ;; TODO: I think the primary behavior of basic creation tests could be
;; ;; abstracted by specifying which things should be allowed in each argument
;; ;; position, and then just generating a bunch of tests from that.
;; (require (for-syntax racket/base
;;                      syntax/parse
;;                      racket/syntax))

;; ;; arg1-types:
;; ;;   - register
;; ;;   - offset
;; ;;   - symbol
;; ;;
;; ;; arg2-types:
;; ;;   - register
;; ;;   - offset
;; ;;   - integer
;; ;;   - shift-width

;; #;(define-syntax (generate-unary-creation-tests stx)
;;   (syntax-parse stx
;;     #:datum-literals (register offset symbol)
;;     [(_ (Constructor) register)
;;      #'(make-test-suite
;;         (format "~a-register-creation-tests" (object-name Constructor))
;;         (for/list ([register-name all-register-names])
;;           (test-not-exn (format "~a with register name ~a"
;;                                 (object-name Constructor)
;;                                 register-name)
;;                         (λ () (Constructor register-name)))))]
;;     [(_ (Constructor) offset)
;;      #'#f]
;;     [(_ (Constructor) symbol)
;;      #'#f]
;;     [(_ (Constructors ...+) arg1-types)
;;      #`(list #,@(map (λ (cs) #`(generate-unary-creation-tests (#,cs) arg1-types))
;;                      (syntax->list #'(Constructors ...))))]))

;; (provide (all-defined-out))

;; (begin-for-syntax
;;   #;(define-syntax-class unary-arg-types
;;     #:description "types of arguments for unary constructors"
;;     #:datum-literals (register offset symbol)
;;     #:attributes (test-register? test-offset? test-symbol?)
;;     (pattern ((~or register offset symbol) ...+)
;;              #:with test-register? )))

;; #;(syntax-parse #'(Foo (offset symbol register))
;;   #:datum-literals (register offset symbol)
;;   [(C ((~or (~optional (~and takes-register? register)) (~optional offset) (~optional symbol)) ...))
;;    (attribute takes-register?)])

;; (define-syntax (generate-unary-creation-tests stx)
;;   (syntax-parse stx
;;     #:datum-literals (register offset symbol)
;;     [(_ Constructor:id ((~or (~optional (~and takes-register? register))
;;                              (~optional (~and takes-symbol? symbol))) ...))
;;      (let* ([Constructor-name (syntax-e #'Constructor)]
;;             [takes-register? (attribute takes-register?)]
;;             [takes-symbol? (attribute takes-symbol?)]
;;             [register-fmt-string (format "~a with register name ~~a" Constructor-name)]
;;             [symbol-fmt-string (format "~a with symbol ~~a" Constructor-name)])
;;        #`(make-test-suite
;;           #,(format "~a-creation-tests" Constructor-name)
;;           (append (for/list ([register-name all-register-names])
;;                     #,(if takes-register?
;;                           #`(test-not-exn (format #,register-fmt-string
;;                                                   register-name)
;;                                           (λ () (Constructor register-name)))
;;                           #`(test-exn (format #,register-fmt-string
;;                                               register-name)
;;                                       exn:fail?
;;                                       (λ () Constructor register-name))))
;;                   (for/list ([valid-name (list 'Constructor 'foo 'entry (gensym 'entry))])
;;                     #,(if takes-symbol?
;;                           #`(test-not-exn (format #,symbol-fmt-string
;;                                                   valid-name)
;;                                           (λ () (Constructor valid-name)))
;;                           #`(test-exn (format #,symbol-fmt-string
;;                                               valid-name)
;;                                       exn:fail?
;;                                       (λ () (Constructor valid-name))))))))]))

;; (begin-for-syntax
;;   (define-syntax-class binary-arg1-type
;;     (pattern (~datum register))
;;     (pattern (~datum offset)))

;;   (define-syntax-class binary-arg2-type
;;     (pattern (~datum register))
;;     (pattern (~datum offset))
;;     (pattern (~datum integer))
;;     (pattern (~datum shift-width)))

;;   (define-syntax-class binary-arg2-types
;;     (pattern ((~or ())))))

;; (define-syntax (generate-binary-creation-tests stx)
;;   (syntax-parse stx
;;     #:datum-literals (register offset integer shift-width)
;;     [(_ Constructor:id (register) ([#:when register (arg2-type:binary-arg2-type ...+)]))
;;      #'#f]

;;     [(_ Constructor:id (register) ([#:when register ((~or (~optional register)
;;                                                           (~optional offset)
;;                                                           (~optional integer)
;;                                                           (~optional shift-width)) ...)]))
;;      #'#f]
;;     [(_ Constructor:id (offset) ([#:when offset ((~or (~optional register)
;;                                                       (~optional offset)
;;                                                       (~optional integer)
;;                                                       (~optional shift-width)) ...)]))
;;      #'#f]


;;     #;[(_ Constructor:id ((~or (~optional register)
;;                              (~optional offset)) ...)
;;         ())]



;;     [(_ Constructor:id
;;         ((~or (~optional (~and arg1-takes-register? register))
;;               (~optional (~and arg1-takes-offset? offset))) ...)
;;         ((~or (~or (~optional [(~seq #:when register) ((~or (~optional (~))))]))
;;               (~or (~optional (~and arg2-takes-register? register))
;;                    (~optional (~and arg2-takes-offset? offset))
;;                    (~optional (~and arg2-takes-integer? integer))
;;                    (~optional (~and arg2-takes-shift-width? shift-width)))) ...))
;;      (let* ([Constructor-name (syntax-e #'Constructor)]
;;             [arg1-takes-register? (attribute arg1-takes-register?)]
;;             [arg1-takes-offset? (attribute arg1-takes-offset?)]
;;             [arg2-takes-register? (attribute arg2-takes-register?)]
;;             [arg2-takes-offset? (attribute arg2-takes-offset?)]
;;             [arg2-takes-integer? (attribute arg2-takes-integer?)]
;;             [arg2-takes-shift-width? (attribute arg2-takes-shift-width?)])
;;        #'#f)]))

;; (define-syntax (define-simple-creation-tests stx)
;;   (syntax-parse stx
;;     #:datum-literals (register symbol)
;;     [(_) #'(begin)]
;;     [(_ [(Constructor:id ...+)
;;          (~and arg1-types ((~or register symbol) ...+))]
;;         clause ...)
;;      #`(list #,@(map (λ (C)
;;                        #`(generate-unary-creation-tests #,C arg1-types))
;;                      (syntax->list #'(Constructor ...))))]





;;     [(_ [constructors
;;          arg1-types]
;;         other-clauses ...)
;;      #'(begin (generate-unary-creation-tests constructors arg1-types)
;;               (define-simple-creation-tests other-clauses ...))]
;;     [(_ [constructors
;;          arg1-types
;;          arg2-types]
;;         other-clauses ...)
;;      #'(begin (generate-binary-creation-tests constructors arg1-types arg2-types)
;;               (define-simple-creation-tests other-clauses ...))]))

;; (define-simple-creation-tests
;;   [(Not Div) (register)]
;;   [(Add Sub)
;;    (register)
;;    (register offset integer)]
;;   [(Mov And Or Xor Cmp)
;;    (register offset)
;;    ([#:when register (register offset integer)]
;;     [#:when offset (register)])]
;;   [(Sal Sar)
;;    (register)
;;    (shift-width)]
;;   [(Call Jmp Je Jne Jl Jle Jg Jge Jo Jno Jc NC)
;;    (symbol)]
;;   [(Cmove Cmovne Cmovl Cmovle Cmovg Cmovge Cmovo Cmovno Cmovc Cmovnc)
;;    (register)
;;    (register offset)])

(define arithmetic-op-creation-tests
  (make-test-suite
   "arithmetic-op-creation-tests"
   (append
    ;; Not
    (list
     (make-test-suite
      "Not-creation-tests"
      (append
       (for/list ([register-name all-register-names])
         (delay-test (test-not-exn (format "Not with register name ~a"
                                           register-name)
                                   (λ () (Not register-name)))))
       (for/list ([symbol-value symbol-values])
         (delay-test (test-exn (format "Not with symbol value ~a"
                                       symbol-value)
                               exn:fail?
                               (λ () (Not symbol-value))))))))
    ;; Add, Sub
    (for/list ([arith-op (list Add Sub)])
      (let ([op-name (object-name arith-op)])
        (make-test-suite
         (format "~a-creation-tests" op-name)
         (flatten
          (for/list ([dst-register all-register-names])
            (for/list ([src-register all-register-names])
              (list (delay-test (test-not-exn (format "~a from register ~a to register ~a"
                                                      op-name src-register dst-register)
                                              (λ () (arith-op dst-register src-register)))))
              (for/list ([i offset-values])
                (list (delay-test (test-not-exn (format "~a from register ~a with offset ~a to register ~a"
                                                        op-name src-register i dst-register)
                                                (λ () (arith-op dst-register (Offset src-register i)))))
                      (delay-test (test-exn (format "~a from register ~a to register ~a with offset ~a"
                                                    op-name src-register dst-register i)
                                            exn:fail?
                                            (λ () (arith-op (Offset dst-register i) src-register))))))
              (for/list ([v immediate-values])
                (delay-test (test-not-exn (format "~a immediate ~a to register ~a"
                                                  op-name v dst-register)
                                          (λ () (arith-op dst-register v)))))))))))
    ;; And, Or, Xor, Cmp
    (for/list ([bitwise-op (list And Or Xor Cmp)])
      (let ([op-name (object-name bitwise-op)])
        (make-test-suite
         (format "~a-creation-tests" op-name)
         (flatten
          (for/list ([dst-register all-register-names])
            (for/list ([src-register all-register-names])
              (list (delay-test (test-not-exn (format "~a from register ~a to register ~a"
                                                      op-name src-register dst-register)
                                              (λ () (bitwise-op dst-register src-register))))
                    (for/list ([i offset-values])
                      (list (delay-test (test-not-exn (format "~a from register ~a with offset ~a to register ~a"
                                                              op-name src-register i dst-register)
                                                      (λ () (bitwise-op dst-register (Offset src-register i)))))
                            (delay-test (test-not-exn (format "~a from register ~a to register ~a with offset ~a"
                                                              op-name src-register dst-register i)
                                                      (λ () (bitwise-op (Offset dst-register i) src-register)))))))))))))
    ;; Sal, Sar
    (for/list ([shift-op (list Sal Sar)])
      (let ([op-name (object-name shift-op)])
        (make-test-suite
         (format "~a-creation-tests" op-name)
         (flatten
          (for/list ([dst-register all-register-names])
            (list (for/list ([i valid-shift-values])
                    (delay-test (test-not-exn (format "~a register ~a by ~a"
                                                      op-name dst-register i)
                                              (λ () (shift-op dst-register i)))))
                  (for/list ([i invalid-shift-values])
                    (delay-test (test-exn (format "~a register ~a by ~a"
                                                  op-name dst-register i)
                                          exn:fail?
                                          (λ () (shift-op dst-register i))))))))))))))

(module+ test
  (run-tests arithmetic-op-creation-tests))

(define jump-op-creation-tests
  (make-test-suite
   "jump-op-creation-tests"
   (for/list ([jump-op (list Jmp Je Jne Jl Jle Jg Jge Jo Jno Jc Jnc)])
     (let ([op-name (object-name jump-op)])
       (make-test-suite
        (format "~a-creation-tests" op-name)
        (flatten
         (list
          (for/list ([register-name all-register-names])
            (list (delay-test (test-not-exn (format "~a to register ~a"
                                                    op-name register-name)
                                            (λ () (jump-op register-name))))
                  (for/list ([i offset-values])
                    (delay-test (test-not-exn (format "~a to register ~a with offset ~a"
                                                      op-name register-name i)
                                              (λ () (jump-op (Offset register-name i))))))))
          (for/list ([target symbol-values])
            (delay-test (test-not-exn (format "~a to target label ~a"
                                              op-name target)
                                      (λ () (jump-op target))))))))))))

(module+ test
  (run-tests jump-op-creation-tests))

(define move-op-creation-tests
  (make-test-suite
   "move-op-creation-tests"
   (append
    ;; Mov
    (list (make-test-suite
           "Mov-creation-tests"
           (flatten
            (for/list ([dst-register all-register-names])
              (list (for/list ([src-register all-register-names])
                      (list (delay-test (test-not-exn (format "Mov to register ~a from register ~a"
                                                              dst-register src-register)
                                                      (λ () (Mov dst-register src-register))))
                            (for/list ([i offset-values])
                              (delay-test (test-not-exn (format "Mov to register ~a from register ~a with offset ~a"
                                                                dst-register src-register i)
                                                        (λ () (Mov dst-register (Offset src-register i)))))
                              (delay-test (test-not-exn (format "Mov to register ~a with offset ~a from register ~a"
                                                                dst-register i src-register)
                                                        (λ () (Mov (Offset dst-register i) src-register))))
                              (for/list ([j offset-values])
                                (delay-test (test-exn (format "Mov to register ~a with offset ~a from register ~a with offset ~a"
                                                              dst-register i
                                                              src-register j)
                                                      exn:fail?
                                                      (λ () (Mov (Offset dst-register i) (Offset src-register j)))))))))
                    (for/list ([v immediate-values])
                      (list (for/list ([i offset-values])
                              (delay-test (test-exn (format "Mov immediate value ~a to register ~a with offset ~a"
                                                            v dst-register i)
                                                    exn:fail?
                                                    (λ () (Mov (Offset dst-register i) v))))))
                      (delay-test (test-not-exn (format "Mov immediate value ~a to register ~a"
                                                        v dst-register)
                                                (λ () (Mov dst-register v))))))))))
    ;; Conditional moves
    (for/list ([cmov-op (list Cmove Cmovne Cmovl Cmovle Cmovg Cmovge
                              Cmovo Cmovno Cmovc Cmovnc)])
      (let ([op-name (object-name cmov-op)])
        (make-test-suite
         (format "~a-creation-tests" op-name)
         (flatten
          (for/list ([dst-register all-register-names])
            (list (for/list ([src-register all-register-names])
                    (list (delay-test (test-not-exn (format "~a to register ~a from register ~a"
                                                            op-name dst-register src-register)
                                                    (λ () (cmov-op dst-register src-register))))
                          (for/list ([i offset-values])
                            (list (delay-test (test-not-exn (format "~a to register ~a from register ~a with offset ~a"
                                                                    op-name dst-register src-register i)
                                                            (λ () (cmov-op dst-register (Offset src-register i)))))
                                  (delay-test (test-exn (format "~a to register ~a with offset ~a from register ~a"
                                                                op-name dst-register i src-register)
                                                        exn:fail?
                                                        (λ () (cmov-op (Offset dst-register i) src-register))))
                                  (for/list ([j offset-values])
                                    (delay-test (test-exn (format "~a to register ~a with offset ~a from register ~a with offset ~a"
                                                                  op-name
                                                                  dst-register i
                                                                  src-register j)
                                                          exn:fail?
                                                          (λ () (cmov-op (Offset dst-register i) (Offset src-register j))))))))))
                  (for/list ([v immediate-values])
                    (list (delay-test (test-exn (format "~a immediate value ~a to register ~a"
                                                        op-name v dst-register)
                                                exn:fail?
                                                (λ () (cmov-op dst-register v))))
                          (for/list ([i offset-values])
                            (delay-test (test-exn (format "~a immediate value ~a to register ~a with offset ~a"
                                                          op-name v dst-register i)
                                                  exn:fail?
                                                  (λ () (cmov-op (Offset dst-register i) v))))))))))))))))

(module+ test
  (run-tests move-op-creation-tests))

(define-test-suite other-op-creation-tests
  (make-test-suite
   "other-op-creation-tests"
   (list
    (make-test-suite
     "Lea-creation-tests"
     (flatten
      (for/list ([dst-register all-register-names])
        (list (for/list ([src-register all-register-names])
                (list (delay-test (test-not-exn (format "Lea into register ~a from register ~a"
                                                        dst-register src-register)
                                                (λ () (Lea dst-register src-register))))
                      (for/list ([i offset-values])
                        (list (delay-test (test-not-exn (format "Lea into register ~a from register ~a with offset ~a"
                                                                dst-register src-register i)
                                                        (λ () (Lea dst-register (Offset src-register i)))))
                              (delay-test (test-not-exn (format "Lea into register ~a with offset ~a from register ~a"
                                                                dst-register i src-register)
                                                        (λ () (Lea (Offset dst-register i) src-register))))
                              (for/list ([j offset-values])
                                (delay-test (test-not-exn (format "Lea into register ~a with offset ~a from register ~a with offset ~a"
                                                                  dst-register i src-register j)
                                                          (λ () (Lea (Offset dst-register i) (Offset src-register j))))))))))
              (for/list ([label symbol-values])
                (delay-test (test-not-exn (format "Lea into register ~a address of label ~a"
                                                  dst-register label)
                                          (λ () (Lea dst-register label))))
                (for/list ([i offset-values])
                  (delay-test (test-not-exn (format "Lea into register ~a with offset ~a address of label ~a"
                                                    dst-register i label)
                                            (λ () (Lea (Offset dst-register i) label))))))
              ;; TODO: Tests for the [exp?] arguments.
              ))))
    (make-test-suite
     "Div-creation-tests"
     (for/list ([register-name all-register-names])
       (delay-test (test-not-exn (format "Div into register ~a" register-name)
                                 (λ () (Div register-name))))))
    (make-test-suite
     "Equ-creation-tests"
     (list
      ;; TODO: Implement these tests.
      ))))
  )

(module+ test
  (run-tests other-op-creation-tests))
