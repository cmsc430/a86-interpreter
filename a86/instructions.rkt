#lang racket

(require "registers.rkt"
         "utility.rkt"

         racket/provide-syntax
         racket/set

         (for-syntax racket/syntax
                     syntax/parse))

(provide seq
         64-bit-integer?
         32-bit-integer?

         ;; Miscellaneous instruction stuff.
         instruction?
         all-instruction-names instruction->instruction-name

         ;; All instructions.
         (instruct-out
          Text Data Rodata Bss
          Label Global Extern
          Push Pop
          Pushf Popf
          Ret Call
          Not Add Sub And Or Xor Cmp Sal Sar
          Jmp Je Jne Jl Jle Jg Jge Jo Jno Jc Jnc
          Mov Cmove Cmovne Cmovl Cmovle Cmovg Cmovge Cmovo Cmovno Cmovc Cmovnc
          Lea Div Equ
          Dd Dq)
         ;; Comment forms.
         comment? % %% %%%
         ;; Offsets.
         Offset offset?
         ;; Constant expressions.
         ;; TODO: Is this necessary?
         (struct-out Const)
         ;; [Lea]-specific.
         Plus lea-exp?
         ;; Symbols and labels.
         label?  ;; TODO: I want a better name.
         symbol->label
         ;; Program construction checker.
         prog
         ;; Convert inputs to lists of instructions.
         read-instructions
         string->instructions)

;; Instructions are provided with their predicates. This allows for
;; pattern-matching and construction, but no field accesses.
(define-provide-syntax (instruct-out stx)
  (syntax-parse stx
    [(_ Name:id ...+)
     (with-syntax ([(Name? ...)
                    (map (λ (name-stx) (format-id name-stx #:source name-stx "~a?" name-stx))
                         (syntax->list #'(Name ...)))])
       #'(combine-out Name ... Name? ...))]))

;; Unfortunately, the checks have to be defined before the use of the
;; [define-instructions] form or else we get undefined identifier errors.
;;
;; TODO: Figure out how to fix that.
;; TODO: These could probably be simplified or combined somehow.

(define check:label-symbol
  (λ (x n)
    (when (register? x)
      (error n "cannot use register as label name; given ~v" x))
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    (unless (label? x)
      (error n "label names must conform to nasm restrictions"))
    x))

(define check:push
  (λ (a1 n)
    (unless (or (exact-integer? a1) (register? a1))
      (error n "expects exact integer or register; given ~v" a1))
    a1))

(define check:register
  (λ (a1 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    a1))

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
    (when (and (exact-integer? a2) (> (integer-length a2) 32))
      (error n "literal must not exceed 32 bits; given ~v (~v bits); go through a register instead"
             a2 (integer-length a2)))
    (values a1 a2)))

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
    (when (and (exact-integer? a2) (> (integer-length a2) 32))
      (error n "literal must not exceed 32 bits; given ~v (~v bits); go through a register instead"
             a2 (integer-length a2)))
    (values a1 a2)))

(define check:shift
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (and (exact-integer? a2) (<= 0 a2 (sub1 word-size-bits)))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,~a]; given ~v" (sub1 word-size-bits) a2))
    (values a1 a2)))

(define check:mov
  (λ (a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2))
      (error n "expects register, offset, exact integer, or defined constant; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (exact-integer? a2) (> (integer-length a2) 64))
      (error n "literal must not exceed 64bits; given ~v (~v bits)" a2 (integer-length a2)))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (values a a1 a2)))

(define check:cmov
  (λ (a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (register? a2) (offset? a2))
      (error n "expects register or offset; given ~v" a2))
    (values a1 a2)))

(define check:lea
  (λ (dst x n)
    (unless (or (register? dst) (offset? dst))
      (error n "expects register or offset; given ~v" dst))
    (unless (or (label? x) (offset? x) (lea-exp? x))
      (error n "expects label, offset, or expression; given ~v" x))
    (values dst x)))

(define check:label-symbol+integer
  (λ (x c n)
    (check:label-symbol x n)
    (unless (integer? c)
      (error n "expects integer constant; given ~v" c))
    (values x c)))

;; Comments also count as instructions for this purpose.
(define (instruction? x)
  (or (Instruction? x)
      (Comment? x)))

;; Nothing special; just some struct.
(struct Instruction () #:transparent)

;; Provides an easy way for defining all of the instructions.
(define-syntax (define-instructions stx)
  (syntax-parse stx
    [(_ all-instruction-names:id
        get-instruction-name:id
        (Name:id (arg:id ...)
                 (~optional guard #:defaults ([guard #'#f]))) ...+)
     (quasisyntax/loc stx
       (begin (define all-instruction-names (list 'Name ...))
              (define (get-instruction-name instruction)
                (match instruction
                  [(struct Name (arg ...)) 'Name] ...))
              #,@(map (λ (name-stx args-stx guard-stx)
                        (quasisyntax/loc name-stx
                          (struct #,name-stx Instruction #,args-stx
                            #:transparent
                            #:guard #,guard-stx)))
                      (syntax->list #'(Name ...))
                      (syntax->list #'((arg ...) ...))
                      (syntax->list #'(guard ...)))))]))

(define-instructions
  all-instruction-names instruction->instruction-name
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
  ;; Flag maintenance.
  (Pushf  ())
  (Popf   ())
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
  (Mov    (dst src) check:mov)
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

;; Comments are accepted anywhere instructions are accepted.
(struct Comment (str)
  #:transparent
  #:guard
  (λ (s n)
    (unless (string? s)
      (error n "expects string; given ~v" s))
    s))
(define comment? Comment?)
(struct %   Comment () #:transparent)  ;; Right side of previous instruction.
(struct %%  Comment () #:transparent)  ;; Indented comment on its own line.
(struct %%% Comment () #:transparent)  ;; Un-indented comment on its own line.

;; Offsets are a subset of operand for some instructions, but they are not
;; instructions themselves.
(struct Offset
  (r i)
  #:transparent
  #:guard (λ (r i n)
            (unless (or (register? r) (label? r))
              (error n "expects register or label as first argument; given ~v" r))
            (unless (exact-integer? i)
              (error n "expects exact integer as second argument; given ~v" i))
            (values r i)))
(define offset? Offset?)

;; NOTE: This is technically part of the x86 specification, but I wonder if we
;; could get away with just removing it?
(struct Const (x) #:transparent)

;; The [Lea] instruction allows for addition in the second operand.
(struct Plus (e1 e2) #:transparent)
;; Checks if something is a valid second operand for [Lea].
(define (lea-exp? x)
  (or (offset? x)
      (and (Plus? x)
           (lea-exp? (Plus-e1 x))
           (lea-exp? (Plus-e2 x)))
      (symbol? x)
      (integer? x)))

;; Any -> Boolean
;; Determines if something is a symbol that can be used as a label in nasm.
(define (label? l)
  (and (symbol? l)
       (not (register? l))
       (nasm-compatible-symbol? l)))

;; Symbol -> Boolean
;; Determines if a symbol conforms to nasm's label naming conventions.
(define (nasm-compatible-symbol? s)
  (regexp-match #rx"^[a-zA-Z._?][a-zA-Z0-9_$#@~.?]*$" (symbol->string s)))

;; Symbol -> Symbol
;; Ensures the given symbol conforms to nasm label naming conventions.
;;
;; TODO: I kind of want to make the conversions a little more expressive, e.g.,
;; maybe something like ['+] becomes ['label_%plus%_1234]. As it is, it just
;; becomes ['label___1234], which can make debugging difficult for students.
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
          (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

;; (U Instruction Asm) ... -> Asm
;; Constructs a "program", by performing global well-formedness checking to help
;; prevent confusing error messages raised by nasm.
;;
;; TODO: I think we might be benefited by merging this and the [Program] from
;; [emulate/program.rkt]. Though, if we do that, we maybe want to give it a
;; custom printer so students can see it nicely.
(define (prog . xs)
  (let ([p (apply seq xs)])
    (check-unique-label-decls p)
    (check-label-targets-declared p)
    (check-has-initial-label p)
    (check-initial-label-global p)
    ;; Any other checks?
    ;; TODO: data/text distinction
    ;; TODO: figure out what the above todo means exactly
    p))

;; Asm -> Void
(define (check-unique-label-decls xs)
  (let ([dupes (check-duplicates (label-decls xs))])
    (when dupes
      (error 'prog "duplicate label declaration found: ~v" dupes))))

;; Asm -> Void
(define (check-label-targets-declared asm)
  (let* ([ds (apply set (label-decls asm))]
         [us (label-uses asm)]
         [undeclared (set-subtract us ds)])
    (unless (set-empty? undeclared)
      (error 'prog "undeclared labels found: ~v" (set->list undeclared)))))

;; Asm -> Void
(define (check-has-initial-label asm)
  (unless (findf Label? asm)
    (error 'prog "no initial label found")))

;; Asm -> Void
(define (check-initial-label-global asm)
  (match (findf Label? asm)
    [(Label init)
     (unless (member init (map (λ (i) (match i [(Global l) l]))
                               (filter Global? asm)))
       (error 'prog "initial label undeclared as global: ~v" init))]))

;; Asm -> (Listof Symbol)
;; Computes all declared label names.
(define (label-decls asm)
  (match asm
    ['() '()]
    [(cons (or (Label  s)
               (Extern s))
           asm)
     (cons s (label-decls asm))]
    [(cons _ asm)
     (label-decls asm)]))

;; Asm -> (Listof Symbol)
;; Computes all uses of label names.
(define (label-uses asm)
  (for/fold ([uses (set)])
            ([i (in-list asm)])
    (match i
      [(or (Call  (? label? s))

           (Jmp   (? label? s))
           (Je    (? label? s))
           (Jne   (? label? s))
           (Jl    (? label? s))
           (Jle   (? label? s))
           (Jg    (? label? s))
           (Jge   (? label? s))
           (Jo    (? label? s))
           (Jno   (? label? s))
           (Jc    (? label? s))
           (Jnc   (? label? s))

           (Lea _ (? label? s)))
       (set-add uses s)]
      [_ uses])))

;; Reads a list of instructions from the current input port. This works by
;; [eval]uating the input in the current namespace.
(define-namespace-anchor a)
(define (read-instructions [in (current-input-port)])
  (let ([ns (namespace-anchor->namespace a)])
    (eval (read in) ns)))

;; Converts a string to a list of instructions.
(define (string->instructions str)
  (read-instructions (open-input-string str)))
