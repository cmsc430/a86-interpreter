#lang racket

(provide compile compile-e)

(require "ast.rkt"
         "compile-ops.rkt"
         "types.rkt"

         "../../../a86/instructions.rkt")

(define rax 'rax)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d)       (compile-value d)]
    [(Prim1 p e)   (compile-prim1 p e)]
    [(If e1 e2 e3) (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ([else (gensym 'if_else)]
        [done (gensym 'if_done)])
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je else)
         (compile-e e2)
         (Jmp done)
         (Label else)
         (compile-e e3)
         (Label done))))
