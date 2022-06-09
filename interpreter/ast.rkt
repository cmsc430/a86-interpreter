#lang racket

(require "registers.rkt"
         racket/set
         (for-syntax syntax/parse))


(define check:label-symbol
  (λ (x n)
    (when (register? x)
      (error n "cannot use register as label name; given ~v" x))
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    x))

(define check:label-symbol+integer
  (λ (x c n)
    (check:label-symbol x n)
    (unless (integer? c)
      (error n "expects integer constant; given ~v" c))
    (values x c)))

(define check:target
  (λ (x n)
    (unless (or (symbol? x) (offset? x)); either register or label
      (error n "expects symbol; given ~v" x))
    x))

(define check:arith
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (exact-integer? a2) (register? a2) (offset? a2))
      (error n "expects exact integer, register, or offset; given ~v" a2))
    (values a1 a2)))

(define check:register
  (λ (a1 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    a1))

(define check:src-dest
  (λ (a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2))
      (error n "expects register, offset, exact integer, or defined constant; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (values a1 a2)))

(define check:shift
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (and (exact-integer? a2) (<= 0 a2 63))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,63]; given ~v" a2))
    (values a1 a2)))

(define check:offset
  (λ (r i n)
    (unless (or (register? r) (label? r))
      (error n "expects register or label as first argument; given ~v" r))
    (unless (exact-integer? i)
      (error n "expects exact integer as second argument; given ~v" i))
    (values r i)))

(define check:push
  (λ (a1 n)
    (unless (or (exact-integer? a1) (register? a1))
      (error n "expects exact integer or register; given ~v" a1))
    a1))

(define check:lea
  (λ (dst x n)
    (unless (or (register? dst) (offset? dst))
      (error n "expects register or offset; given ~v" dst))
    ;; TODO: Implement this part.
    #;(unless (or (label? x) (offset? x) (exp? x))
      (error n "expects label, offset, or expression; given ~v" x))
    (values dst x)))

(define check:none
  (λ (n) (values)))

(struct Instruction () #:transparent)
(define instruction? Instruction?)

(define-syntax-rule
  (define-instruction (Name (fields ...) guard))
  (begin (provide (struct-out Name))
         (struct Name Instruction (fields ...)
           #:transparent
           #:guard guard)))

(define-syntax (define-instructions stx)
  (syntax-parse stx
    [(_ instruction)
     #'(define-instruction instruction)]
    [(_ instruction instructions ...+)
     #'(begin (define-instruction instruction)
              (define-instructions instructions ...))]))

(define-instructions
  (Label (x)       check:label-symbol)
  (Ret   ()        check:none)
  (Call  (x)       check:target)
  (Mov   (dst src) check:src-dest)
  (Add   (dst src) check:arith)
  (Sub   (dst src) check:arith)
  (Cmp   (a1 a2)   check:src-dest)
  (Jmp   (x)       check:target)
  (Je    (x)       check:target)
  (Jne   (x)       check:target)
  (Jl    (x)       check:target)
  (Jg    (x)       check:target)
  (And   (dst src) check:src-dest)
  (Or    (dst src) check:src-dest)
  (Xor   (dst src) check:src-dest)
  (Sal   (dst i)   check:shift)
  (Sar   (dst i)   check:shift)
  (Push  (a1)      check:push)
  (Pop   (a1)      check:register)
  (Lea   (dst x)   check:lea))

(provide (struct-out Offset))
(struct Offset (r i) #:transparent)
(provide offset?)
(define offset? Offset?)

(provide (struct-out Const))
(struct Const (x) #:transparent)

(define (label? l)
  (and (symbol? l)
       (not (register? l))))

;; A Program is a list of instructions.
;;
;; TODO: It would be neat to define, say, [#lang a86] where the program can just
;; be directly written, and then the underlying implementation of the #lang
;; converts it into a [Program]. Don't know how practical or useful that is.
(provide (struct-out Program))
(struct Program (instructions)
  #:transparent
  #:guard (λ (instructions n)
            (unless (list? instructions)
              (error n "instructions must be given as a list"))
            (unless (not (empty? instructions))
              (error n "must be given at least one instruction"))
            (unless (Label? (first instructions))
              (error n "first instruction must be a label; given ~v" (first instructions)))
            (for/fold ([labels (set)])
                      ([instruction instructions])
              (unless (Instruction? instruction)
                (error n "all instructions must be valid; given ~v" instruction))
              (if (Label? instruction)
                  (begin
                    (when (set-member? labels (Label-x instruction))
                      (error n "labels cannot be repeated; given ~v" (Label-x instruction)))
                    (set-add labels (Label-x instruction)))
                  labels))
            instructions))
