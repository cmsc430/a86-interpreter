#lang racket

(require "interpreter/registers.rkt"
         "interpreter/utility.rkt"
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
    (unless (or (and (exact-integer? a2) (<= 0 a2 (sub1 (word-size-bits))))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,~a]; given ~v" (sub1 (word-size-bits)) a2))
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
    ;; TODO: Do we need this part? The [exp?] predicate has to do with the
    ;; [Plus] struct in the original a86, but I can't figure what [Plus] is
    ;; actually for.
    #;(unless (or (label? x) (offset? x) (exp? x))
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
  ;; Separate the input.
  (Text   ())
  (Data   ())
  ;; Distinguish a subset of instructions.
  (Label  (x)       check:label-symbol)
  (Global (x)       check:label-symbol)
  (Extern (x)       check:label-symbol)
  ;; Regular instructions.
  (Ret    ())
  (Call   (x)       check:target)
  (Mov    (dst src) check:src-dest)
  (Add    (dst src) check:arith)
  (Sub    (dst src) check:arith)
  (Cmp    (a1 a2)   check:src-dest)
  (Jmp    (x)       check:target)
  (Je     (x)       check:target)
  (Jne    (x)       check:target)
  (Jl     (x)       check:target)
  (Jle    (x)       check:target)
  (Jg     (x)       check:target)
  (Jge    (x)       check:target)
  (And    (dst src) check:src-dest)
  (Or     (dst src) check:src-dest)
  (Xor    (dst src) check:src-dest)
  (Sal    (dst i)   check:shift)
  (Sar    (dst i)   check:shift)
  (Push   (a1)      check:push)
  (Pop    (a1)      check:register)
  (Lea    (dst x)   check:lea)
  (Div    (den)     check:register)
  (Equ    (x v)     check:label-symbol+integer)
  ;; Data instructions.
  (Dd     (x))   ;; Define Double word.
  (Dq     (x)))  ;; Define Quad word.

;; TODO: What is this? Is it necessary?
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
(struct Offset (r i) #:transparent)
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

;; A Program is a list of instructions with a defined entry point.
;;
;; TODO: It would be neat to define, say, [#lang a86] where the program can just
;; be directly written, and then the underlying implementation of the #lang
;; converts it into a [Program]. Don't know how practical or useful that is.
(provide (struct-out Program))
(struct Program (instructions entry-point labels globals externs) #:transparent)

(define (initialize-program instructions entry-point)
  (when (instruction? instructions)
    (set! instructions (list instructions)))
  (unless (list? instructions)
    (raise-user-error 'program "not a list: ~v" instructions))
  (unless (not (empty? instructions))
    (raise-user-error 'program "must be given at least one instruction"))
  (unless (or (symbol? entry-point)
              (eq? #f entry-point))
    (raise-user-error 'program "entry point not a symbol: ~v" entry-point))
  (let*-values ([(first-global) #f]
                [(labels globals externs)
                 (for/fold ([labels (set)]
                            [globals (set)]
                            [externs (set)])
                           ([instruction instructions])
                   (unless (instruction? instruction)
                     (raise-user-error 'program "not an instruction: ~v" instruction))
                   (match instruction
                     [(Label l)
                      (when (set-member? labels l)
                        (raise-user-error 'program "repeated internal label: ~v" l))
                      (when (set-member? externs l)
                        (raise-user-error 'program "internal label was previously declared external: ~v" l))
                      (values (set-add labels l) globals externs)]
                     [(Global l)
                      (when (set-member? globals l)
                        (raise-user-error 'program "repeated global declaration: ~v" l))
                      (unless first-global
                        (set! first-global l))
                      (values labels (set-add globals l) externs)]
                     [(Extern l)
                      (when (set-member? externs l)
                        (raise-user-error 'program "repeated external label: ~v" l))
                      (when (set-member? labels l)
                        (raise-user-error 'program "external label was previously declared internal: ~v" l))
                      (values labels globals (set-add externs l))]
                     [_ (values labels globals externs)]))])
    (when (set-empty? labels)
      (raise-user-error 'program "program must contain at least one internal label"))
    (when (set-empty? globals)
      (raise-user-error 'program "program must contain at least one global declaration"))
    (for ([g globals])
      (unless (set-member? labels g)
        (raise-user-error 'program "global declaration lacks corresponding internal label: ~v" g)))
    (when (eq? #f entry-point)
      (set! entry-point first-global))
    (unless (set-member? globals entry-point)
      (raise-user-error 'program "missing global declaration for specified program entry point: ~v" entry-point))
    (Program instructions entry-point labels globals externs)))

(provide define-program)
;; Processes a given list of instructions to produce a Program.
(define-syntax (define-program stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:entry-point entry-point) #:defaults ([entry-point #'#f]))
        instructions)
     #'(initialize-program instructions entry-point)]
    [(_ (~optional (~seq #:entry-point entry-point) #:defaults ([entry-point #'#f]))
        instruction ...+)
     #'(initialize-program (list instruction ...) entry-point)]))
