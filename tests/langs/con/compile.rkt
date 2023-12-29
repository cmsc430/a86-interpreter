#lang racket

(provide compile compile-e)

(require "ast.rkt"
         "compile-prim.rkt"

         "../../../a86/instructions.rkt")

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit i)           (compile-integer i)]
    [(Prim1 p e)       (compile-prim1 p (compile-e e))]
    [(IfZero e1 e2 e3) (compile-ifzero e1 e2 e3)]))

;; Integer -> Asm
(define (compile-integer i)
  (seq (Mov 'rax i)))

;; Expr Expr Expr -> Asm
(define (compile-ifzero e1 e2 e3)
  (let ([not-zero (gensym 'ifz_nz)]
        [yes-zero (gensym 'ifz_yz)])
    (seq (compile-e e1)
         (Cmp 'rax 0)
         (Jne not-zero)
         (compile-e e2)
         (Jmp yes-zero)
         (Label not-zero)
         (compile-e e3)
         (Label yes-zero))))
