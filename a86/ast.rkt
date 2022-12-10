#lang racket

(require "registers.rkt"
         "utility.rkt"
         racket/set
         (for-syntax syntax/parse))

(provide (rename-out [sequence seq])
         64-bit-integer?
         32-bit-integer?)

(define check:label-symbol
  (λ (x n)
    (when (register? x)
      (error n "cannot use register as label name; given ~v" x))
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    (unless (label? x)
      (error n "label names must conform to nasm restrictions"))
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

(define check:cmov
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (register? a2) (offset? a2))
      (error n "expects register or offset; given ~v" a2))
    (values a1 a2)))

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
    (unless (or (and (exact-integer? a2) (<= 0 a2 (sub1 word-size-bits)))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,~a]; given ~v" (sub1 word-size-bits) a2))
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
    (unless (or (label? x) (offset? x) (exp? x))
      (error n "expects label, offset, or expression; given ~v" x))
    (values dst x)))

(define check:none
  (λ (n) (values)))

(provide (struct-out %)
         (struct-out %%)
         (struct-out %%%)
         Comment?)

(struct Comment (str)
  #:transparent
  #:guard
  (λ (s n)
    (unless (string? s)
      (error n "expects string; given ~v" s))
    s))

(struct %   Comment () #:transparent)
(struct %%  Comment () #:transparent)
(struct %%% Comment () #:transparent)

(struct Instruction () #:transparent)
(provide instruction?)
(define (instruction? x)
  (or (Instruction? x)
      (Comment? x)))

(define-syntax (define-instructions stx)
  (syntax-parse stx
    [(_ (Name (arg ...) (~optional guard #:defaults ([guard #'#f]))) ...+)
     #'(begin (provide (struct-out Name) ...)
              (struct Name Instruction (arg ...) #:transparent #:guard guard) ...
              (provide get-instruction-name)
              (define (get-instruction-name instruction)
                (match instruction
                  [(struct Name (arg ...)) 'Name] ...)))]))

(define-instructions
  ;; Separate the input by section.
  (Text   ())
  (Data   ())
  (Rodata ())
  (Bss    ())
  ;; Distinguish a subset of instructions.
  (Label  (x)       check:label-symbol)
  (Global (x)       check:label-symbol)
  (Extern (x)       check:label-symbol)
  ;; Stack maintenance.
  (Push   (a1)      check:push)
  (Pop    (a1)      check:register)
  ;; Function calls.
  (Ret    ())
  (Call   (x)       check:target)
  ;; Arithmetic operations.
  (Not    (x)       check:register)
  (Add    (dst src) check:arith)
  (Sub    (dst src) check:arith)
  (And    (dst src) check:src-dest)
  (Or     (dst src) check:src-dest)
  (Xor    (dst src) check:src-dest)
  (Cmp    (a1 a2)   check:src-dest)
  (Sal    (dst i)   check:shift)
  (Sar    (dst i)   check:shift)
  ;; Jumps.
  (Jmp    (x)       check:target)
  (Je     (x)       check:target)
  (Jne    (x)       check:target)
  (Jl     (x)       check:target)
  (Jle    (x)       check:target)
  (Jg     (x)       check:target)
  (Jge    (x)       check:target)
  (Jo     (x)       check:target)
  (Jno    (x)       check:target)
  (Jc     (x)       check:target)
  (Jnc    (x)       check:target)
  ;; Moves.
  (Mov    (dst src) check:src-dest)
  (Cmove  (dst src) check:cmov)
  (Cmovne (dst src) check:cmov)
  (Cmovl  (dst src) check:cmov)
  (Cmovle (dst src) check:cmov)
  (Cmovg  (dst src) check:cmov)
  (Cmovge (dst src) check:cmov)
  (Cmovo  (dst src) check:cmov)
  (Cmovno (dst src) check:cmov)
  (Cmovc  (dst src) check:cmov)
  (Cmovnc (dst src) check:cmov)
  ;; Other instructions.
  (Lea    (dst x)   check:lea)
  (Div    (den)     check:register)
  (Equ    (x v)     check:label-symbol+integer)
  ;; Data instructions.
  (Dd     (x))   ;; Define Double word.
  (Dq     (x)))  ;; Define Quad word.

;; This is used for the Lea instruction, which allows for addition in the
;; second operand.
(provide (struct-out Plus))
(struct Plus (e1 e2) #:transparent)

(provide exp?)
(define (exp? x)
  (or (Offset? x)
      (and (Plus? x)
           (exp? (Plus-e1 x))
           (exp? (Plus-e2 x)))
      (symbol? x)
      (integer? x)))

(provide Offset)
(struct
  Offset
  (r i)
  #:transparent
  #:guard (λ (r i n)
            (unless (or (register? r) (label? r))
              (error n "expects register or label as first argument; given ~v" r))
            (unless (exact-integer? i)
              (error n "expects exact integer as second argument; given ~v" i))
            (values r i)))
(provide offset offset?)
(define (offset r [i 0]) (Offset r i))
(define offset? Offset?)
(provide address-from-offset)
;; Calculates an offset address.
(define (address-from-offset registers offset)
  (+ (Offset-i offset) (hash-ref registers (Offset-r offset))))

(provide (struct-out Const))
(struct Const (x) #:transparent)

(provide label?)
(define (label? l)
  (and (symbol? l)
       (nasm-label? l)
       (not (register? l))))

(provide label-type?)
(define (label-type? instruction)
  (or (Label? instruction)
      (Global? instruction)
      (Extern? instruction)))

(provide get-label)
(define (get-label instruction)
  (match instruction
    [(Label x) x]
    [(Global x) x]
    [(Extern x) x]))

(provide prog)
;; (U Instruction Asm) ... -> Asm
;; Construct a "program", does some global well-formedness checking to help
;; prevent confusing error messages as the nasm level
(define (prog . xs)
  (let ((p (apply sequence xs)))
    (check-unique-label-decls p)
    (check-label-targets-declared p)
    (check-has-initial-label p)
    ;; anything else?
    ;; TODO: data/text distinction
    p))

;; Asm -> Void
(define (check-unique-label-decls xs)
  (let ((r (check-duplicates (label-decls xs))))
    (when r
      (error 'prog "duplicate label declaration found: ~v" r))))

;; Asm -> (Listof Symbol)
;; Compute all declared label names
(define (label-decls asm)
  (match asm
    ['() '()]
    [(cons (Label s) asm)
     (cons s (label-decls asm))]
    [(cons (Extern s) asm)
     (cons s (label-decls asm))]
    [(cons _ asm)
     (label-decls asm)]))

;; Symbol -> Boolean
(define (nasm-label? s)
  (regexp-match #rx"^[a-zA-Z._?][a-zA-Z0-9_$#@~.?]*$" (symbol->string s)))

;; Asm -> (Listof Symbol)
;; Compute all uses of label names
(define (label-uses asm)
  (match asm
    ['() '()]
    [(cons (Jmp (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Je (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jne (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jg (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jge (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jl (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Jle (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Call (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons (Lea _ (? label? s)) asm)
     (cons s (label-uses asm))]
    [(cons _ asm)
     (label-uses asm)]))


;; Asm -> Void
(define (check-label-targets-declared asm)
  (let ((ds (apply set (label-decls asm)))
        (us (apply set (label-uses asm))))

    (let ((undeclared (set-subtract us ds)))
      (unless (set-empty? undeclared)
        (error 'prog "undeclared labels found: ~v" (set->list undeclared))))))

;; Asm -> Void
(define (check-has-initial-label asm)
  (unless (findf Label? asm)
    (error 'prog "no initial label found")))
