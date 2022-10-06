#lang racket
(provide/contract
 [asm-string  (-> (listof instruction?) string?)] ; deprecated
 [asm-display (-> (listof instruction?) any)])

(define current-shared?
  (make-parameter #f))

(module* private #f
  (provide current-shared?))

(require "ast.rkt"
         (only-in "interpreter/registers.rkt" register?))

;; Any -> Boolean
(define (reg? x)
  (register? x))

;; Reg -> String
(define (reg->string r)
  (symbol->string r))

;; Asm -> String
(define (asm-string a)
  (with-output-to-string (lambda () (asm-display a))))

;; Asm -> Void
(define (asm-display a)
  (define external-labels '())

  ;; Label -> String
  ;; prefix with _ for Mac
  (define label-symbol->string
    (match (system-type 'os)
      ['macosx
       (λ (s) (string-append "_" (symbol->string s)))]
      [_
       (if (current-shared?)
           (λ (s)
                  (if (memq s external-labels)
                      ; hack for ELF64 shared libraries in service of
                      ; calling external functions in asm-interp
                      (string-append (symbol->string s) " wrt ..plt")
                      (symbol->string s)))
           symbol->string)]))

  ;; (U Label Reg) -> String
  (define (jump-target->string t)
    (match t
      [(? reg?) (reg->string t)]
      [(Offset (? reg? r) i)
       (string-append "[" (reg->string r) " + " (number->string i) "]")]
      [_ (label-symbol->string t)]))

  ;; Arg -> String
  (define (arg->string a)
    (match a
      [(? reg?) (reg->string a)]
      [(? integer?) (number->string a)]
      [(Offset (? reg? r) i)
       (string-append "[" (reg->string r) " + " (number->string i) "]")]
      [(Offset (? label? l) i)
       (string-append "[" (label-symbol->string l) " + " (number->string i) "]")]
      [(Const l)
       (symbol->string l)]
      [(? exp?) (exp->string a)]))

  ;; Exp -> String
  (define (exp->string e)
    (match e
      [(? integer?) (number->string e)]
      [(Plus e1 e2)
       (string-append "(" (exp->string e1) " + " (exp->string e2) ")")]
      [_ (label-symbol->string e)]))

  (define tab (make-string 8 #\space))

  ;; Instruction -> String
  (define (instr->string i)
    (match i
      [(Text)      (string-append tab "section .text")]
      [(Data)      (string-append tab "section .data align=8")] ; 8-byte aligned data
      [(Ret)       (string-append tab "ret")]
      [(Label l)   (string-append (label-symbol->string l) ":")]
      [(Global x)  (string-append tab "global "  (label-symbol->string x))]
      [(Extern l)  (begin0 (string-append tab "extern " (label-symbol->string l))
                           (set! external-labels (cons l external-labels)))]
      [(Mov a1 a2)
       (string-append tab "mov "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Add a1 a2)
       (string-append tab "add "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Sub a1 a2)
       (string-append tab "sub "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Cmp a1 a2)
       (string-append tab "cmp "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Sal a1 a2)
       (string-append tab "sal "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Sar a1 a2)
       (string-append tab "sar "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(And a1 a2)
       (string-append tab "and "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Or a1 a2)
       (string-append tab "or "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Xor a1 a2)
       (string-append tab "xor "
                      (arg->string a1) ", "
                      (arg->string a2))]
      [(Jmp l)
       (string-append tab "jmp "
                      (jump-target->string l))]
      [(Je l)
       (string-append tab "je "
                      (jump-target->string l))]
      [(Jne l)
       (string-append tab "jne "
                      (jump-target->string l))]
      [(Jl l)
       (string-append tab "jl "
                      (jump-target->string l))]
      [(Jle l)
       (string-append tab "jle "
                      (jump-target->string l))]
      [(Jg l)
       (string-append tab "jg "
                      (jump-target->string l))]
      [(Jge l)
       (string-append tab "jge "
                      (jump-target->string l))]
      [(Call l)
       (string-append tab "call "
                      (jump-target->string l))]
      [(Push a)
       (string-append tab "push "
                      (arg->string a))]
      [(Pop r)
       (string-append tab "pop "
                      (reg->string r))]
      [(Lea d (? offset? x))
       (string-append tab "lea "
                      (arg->string d) ", "
                      (arg->string x))]
      [(Lea d x)
       (string-append tab "lea "
                      (arg->string d) ", [rel "
                      (exp->string x) "]")]
      [(Div r)
       (string-append tab "div "
                      (arg->string r))]
      [(Equ x c)
       (string-append tab
                      (symbol->string x)
                      " equ "
                      (number->string c))]

      [(Dd x)
       (string-append tab "dd " (arg->string x))]
      [(Dq x)
       (string-append tab "dq " (arg->string x))]
      ))

  (define (comment->string c)
    (match c
      [(% s)   (string-append (make-string 32 #\space) "; " s)]
      [(%% s)  (string-append tab ";; " s)]
      [(%%% s) (string-append ";;; " s)]))

  (define (line-comment i s)
    (let ((i-str (instr->string i)))
      (let ((pad (make-string (max 1 (- 32 (string-length i-str))) #\space)))
        (string-append i-str pad "; " s))))

  ;; [Listof Instr] -> Void
  (define (instrs-display a)
    (match a
      ['() (void)]
      [(cons (? Comment? c) a)
       (begin (write-string (comment->string c))
              (write-char #\newline)
              (instrs-display a))]
      [(cons i (cons (% s) a))
       (begin (write-string (line-comment i s))
              (write-char #\newline)
              (instrs-display a))]
      [(cons i a)
       (begin (write-string (instr->string i))
              (write-char #\newline)
              (instrs-display a))]))

  ;; entry point will be first label
  (match (findf Label? a)
    [(Label g)
     (begin
       (write-string (string-append
                      tab "global " (label-symbol->string g) "\n"
                      tab "default rel\n"
                      tab "section .text\n"))
       (instrs-display a))]
    [_
     (instrs-display a)
     #;
     (error "program does not have an initial label")]))
