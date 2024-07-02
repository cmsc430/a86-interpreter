#lang racket

(provide format/repl

         current-memory-value-width

         current-escape-formatter/~f
         current-escape-formatter/~r
         current-escape-formatter/~r*
         current-escape-formatter/~i
         current-escape-formatter/~m
         current-escape-formatter/~F
         current-escape-formatter/~R
         current-escape-formatter/~R*
         current-escape-formatter/~I
         current-escape-formatter/~M

         define-show

         format-memory)

(require "../../registers.rkt"
         "../../utility.rkt"
         "exn.rkt"
         "repl-state.rkt"

         (for-syntax (submod "../../utility.rkt" racket/syntax)
                     racket/format
                     racket/list
                     racket/string
                     syntax/parse
                     syntax/parse/lib/function-header))

(define-syntax (define-format-for-show stx)

  (define base-monochar-escapes
    (list "a" "A" "s" "S" "v" "V"
          "e" "E" "c" "C"
          "b" "B" "o" "O" "x" "X"))
  (define base-multichar-escapes
    (list ".a" ".A" ".s" ".S" ".v" ".V"))

  (define (separate-escapes escape-ids)
    (partition
     (λ (e-str) (= 1 (string-length e-str)))
     (map (λ (e-id) (~a (syntax-e e-id))) escape-ids)))

  (define (build-regexp-from-escapes monochar multichar)
    (regexp (~a "(?<=[^~]|^)(?:~~)*~("
                (string-join (append (map regexp-quote (sort multichar
                                                             (λ (l r)
                                                               (> (string-length l)
                                                                  (string-length r)))))
                                     (if (empty? monochar)
                                         '()
                                         (list "["
                                               (apply string-append monochar)
                                               "]")))
                             "|")
                ")")))

  (syntax-parse stx
    [(_ format-func-name:id
        [escape:id retrieval-proc:expr
                   (~optional formatter-proc:expr #:defaults ([formatter-proc #'((curry ~a) #:separator "/")]))] ...+)

     (let*-values ([(escape-ids) (syntax->list #'(escape ...))]
                   [(new-monochar-escapes new-multichar-escapes)
                    (separate-escapes escape-ids)]
                   [(all-monochar-escapes)  (append base-monochar-escapes  new-monochar-escapes)]
                   [(all-multichar-escapes) (append base-multichar-escapes new-multichar-escapes)])
       (with-syntax
         ([(current-escape-retriever-name ...) (format-ids escape-ids "current-escape-retriever/~~~a")]
          [(current-escape-formatter-name ...) (format-ids escape-ids "current-escape-formatter/~~~a")]
          [(escape/id?                    ...) (format-ids escape-ids "escape/~~~a?")]
          [escape-regexp                       (build-regexp-from-escapes all-monochar-escapes
                                                                          all-multichar-escapes)])

         #'(begin
             (define current-escape-retriever-name
               (make-parameter retrieval-proc)) ...
             (define current-escape-formatter-name
               (make-parameter formatter-proc)) ...

             (define format-func-name
               (let ()
                 (define (split-input input)
                   (map (match-lambda [(list s) (string->symbol s)]
                                      [v v])
                        (regexp-match* escape-regexp
                                       input
                                       #:match-select cdr
                                       #:gap-select?   #t)))
                 (define (escape/id? x) (and (symbol? x)
                                             (eq? x 'escape))) ...
                 (define (format-func-name form . vs)
                   (for/fold ([new-form-parts '()]
                              [new-vs         '()]
                              [old-vs          vs]
                              #:result
                              (let ([fmt-str (string-append* (reverse new-form-parts))]
                                    [all-vs  (append (reverse new-vs)
                                                     old-vs)])
                                (with-handlers ([exn:fail:contract:bad-format?
                                                 (λ (e)
                                                   (let ([n-expected (exn:fail:contract:bad-format-expected-args e)])
                                                     (raise-a86-user-repl-format-bad-arg-count-error
                                                      (length all-vs)
                                                      n-expected
                                                      all-vs
                                                      'format-func-name
                                                      "format string requires ~a arguments, given ~a\n  arguments were: ~a\n  format string: ~v"
                                                      n-expected
                                                      (length all-vs)
                                                      (string-join (map ~s all-vs)
                                                                   " ")
                                                      fmt-str)))])
                                  (apply format
                                         fmt-str
                                         all-vs))))
                             ([part (in-list (split-input form))])
                     (cond [(escape/id? part)
                            (let*-values ([(retriever)     (current-escape-retriever-name)]
                                          [(now-vs old-vs) (split-at old-vs (procedure-arity retriever))]
                                          [(new-v)         (apply retriever now-vs)]
                                          [(formatter)     (current-escape-formatter-name)])
                              (values (cons "~a" new-form-parts)
                                      (cons (apply formatter new-v now-vs) new-vs)
                                      old-vs))] ...
                           [(symbol? part)
                            (values (cons (format "~~~a" part) new-form-parts)
                                    (cons (car old-vs) new-vs)
                                    (cdr old-vs))]
                           [(string? part)
                            (values (cons part new-form-parts)
                                    new-vs
                                    old-vs)]
                           [else (raise-user-error 'format-func-name
                                                   "could not parse form: ~v"
                                                   part)])))
                 format-func-name)))))]))

(define current-memory-value-width (make-parameter 20))

;; Formats a string similar to [format], but with custom format escapes for the
;; a86 REPL.
;;
;; The standard escapes all work as they usually do in [format]:
;;   * [~n] or [~%] --- newline
;;   * [~a] or [~A] --- [display]s the next argument
;;   * [~s] or [~S] --- [write]s the next argument
;;   * [~v] or [~V] --- [print]s the next argument
;;   * [~~]         --- literal tilde
;;
;; Custom escapes are added for use in the REPL:
;;   * [~f]  or [~F]  --- (curr/prev) flags
;;   * [~f*] or [~F*] --- (curr/prev) flags (all four)
;;   * [~r]  or [~R]  --- (curr/prev) register
;;   * [~r*] or [~R*] --- (curr/prev) registers (list of registers)
;;   * [~r&] or [~R&] --- (curr/prev) register dereference
;;   * [~i]  or [~I]  --- (curr/prev) instruction
;;   * [~m]  or [~M]  --- (curr/prev) memory address
;;   * [~l]  or [~L]  --- label of address OR address of label
;;
;; Simple Mode format:
;;   (format-show "[~f] [~r*] ~i"
;;                (current-repl-modified-flags)
;;                (current-repl-modified-registers)
;;                (current-repl-instruction))
(define-format-for-show format/repl
  ;; Current flag value, where lowercase represents unset and uppercase is set.
  [f current-repl-flag-ref
     (λ (fv f) (let* ([f-str (~a f)]
                      [f-chr (substring f-str 0 1)])
                 (if fv
                     (string-upcase   f-chr)
                     (string-downcase f-chr))))]
  ;; Current flag value, given as either [#t] or [#f].
  [f! current-repl-flag-ref
      (λ (fv _) (~a fv))]
  ;; All current flag values.
  [f* (λ () (map current-repl-flag-ref flag-names))
      (λ (fvs) (string-append* (map (current-escape-formatter/~f) fvs flag-names)))]
  ;; Current register value.
  [r current-repl-register-ref
     (λ (rv _) (~a rv))]
  ;; Multiple current register values.
  [r* (λ (rs) (map current-repl-register-ref rs))
      (λ (rvs _) (string-join (map (current-escape-formatter/~r) rvs) ", "))]
  ;; Current value in memory at register address. The argument can be a register
  ;; or a list of a register with an integer, in which case the integer is
  ;; treated as a byte offset.
  [r& (λ (r+o)
        (match r+o
          [(or (? register? r)
               (list (? register? r)))
           (current-repl-memory-ref (current-repl-register-ref r))]
          [(list (? register? r) (? exact-integer? o))
           (current-repl-memory-ref (+ o (current-repl-register-ref r)))]))
      (λ (mv x) ((current-escape-formatter/~m) mv x))]
  ;; Current instruction.
  [i current-repl-instruction
     ~v]
  ;; Current value at a memory address.
  [m current-repl-memory-ref
     (λ (mv _)
       (~a "0x"
           (~r mv
               #:base 16
               #:min-width 16
               #:pad-string "0")
           #:width (current-memory-value-width)
           #:align 'right))]
  ;; Current step index.
  [q current-repl-emulator-state-index]
  ;; Current remaining input in the input string.
  [<< (λ ()
        (or (current-repl-input-port->string)
            ""))]
  ;; The full (original) input string.
  [<<< (λ ()
         (or (current-repl-input-port->string #:from-beginning? #t)
             ""))]
  ;; The current position in the input string, graphically.
  [<<^ (λ ()
         (or (and (current-repl-input-port->string)
                  (let ([pos (file-position* (current-repl-input-port))])
                    (~a "^"
                        #:width (add1 pos)
                        #:pad-string "~"
                        #:align 'right)))
             ""))]
  ;; Current output string.
  [>> (λ ()
        (or (current-repl-output-port->string)
            ""))]
  ;; Previous flag value, where lowercase represents unset and uppercase is set.
  [F previous-repl-flag-ref
     (λ (fv _) ((current-escape-formatter/~f) fv))]
  ;; Previous flag value, given as either [#t] or [#f].
  [F! previous-repl-flag-ref
      (λ (fv _) ((current-escape-formatter/~f!) fv))]
  ;; All previous flag values.
  [F* (λ () (map previous-repl-flag-ref flag-names))
      (λ (fvs) ((current-escape-formatter/~f*) fvs))]
  ;; Previous register value.
  [R previous-repl-flag-ref
     (λ (rv _) ((current-escape-formatter/~r) rv))]
  ;; Multiple previous register values.
  [R* (λ (rs) (map previous-repl-register-ref rs))
      (λ (rvs _) ((current-escape-formatter/~r*) rvs))]
  ;; Value in memory at register address in previous step of interpreter. The
  ;; argument can be a register or a list of a register with an integer, in
  ;; which case the integer is treated as a byte offset.
  [R& (λ (r+o)
        (match r+o
          [(or (? register? r)
               (list (? register? r)))
           (previous-repl-memory-ref (previous-repl-register-ref r))]
          [(list (? register? r) (? exact-integer? o))
           (previous-repl-memory-ref (+ o (previous-repl-register-ref r)))]))
      (λ (mv x) ((current-escape-formatter/~r&) mv x))]
  ;; Previous instruction.
  [I previous-repl-instruction
     (λ (iv) ((current-escape-formatter/~i) iv))]
  ;; Previous value at a memory address.
  [M previous-repl-memory-ref
     (λ (mv _) ((current-escape-formatter/~m) mv))]
  ;; Previous step index.
  [Q previous-repl-emulator-state-index])

(define-syntax (define-show stx)
  (define-syntax-class arg-list
    (pattern (es ...))
    (pattern e #:attr [es 1] (list #'e)))

  (define-syntax-class string*
    (pattern s:string)
    (pattern (strings:string ...+)
             #:attr s #`#,(string-join (map syntax-e
                                            (syntax->list #'(strings ...)))
                                       "")))

  (syntax-parse stx
    [(_ (name:id . formals:formals)
        (~optional (~seq #:sep sep))
        (~or* [          arg-str:string* (~optional args:arg-list)]
              [condition arg-str:string* (~optional args:arg-list)]
              arg-str:string*                                      ) ...)

     #`(define (name . formals)
         (for/fold ([string-parts '()]
                    [arguments    '()]
                    #:result (displayln
                              (apply format/repl
                                     (string-join (reverse string-parts) (~? sep "\n"))
                                     (reverse arguments))))
                   ([cond-thunk     (list (λ () (~? condition #t)) ...)]
                    [string-part    (list arg-str.s ...)]
                    [arg-list-thunk (list (λ () (list (~? (~@ args.es ...)))) ...)])
           (if (cond-thunk)
               (values (cons string-part string-parts)
                       (append (reverse (arg-list-thunk)) arguments))
               (values string-parts
                       arguments))))]))

;; Returns a string formatting values in memory with their addresses, where the
;; high addresses are displayed at the top, and each word is visually divided
;; with borders.
(define (format-memory base n)
  (unless (not (zero? n))
    (raise-argument-error 'format-memory "non-zero integer?" n))
  (let*-values ([(base-address) (match base
                                  [(? register?) (current-repl-register-ref base)]
                                  [(? address?)  base])]
                [(as+vses max-string-length max-address-length)
                 (for/fold ([as+vses            '()]
                            [max-string-length    0]
                            [max-address-length   0])
                           ([i (in-range (abs n))])
                   ;; We want the high addresses to appear first, so if [n] is
                   ;; positive we start with the greatest offset, and if [n] is
                   ;; negative we start with the zero offset.
                   (let* ([a (if (positive? n)
                                 (+ base-address (*  8 (- n (add1 i))))
                                 (+ base-address (* -8 i)))]
                          [vs (format/repl "~m" a)]
                          [as (~r a #:base 16)])
                     (values (cons (cons as vs) as+vses)
                             (max max-string-length  (string-length vs))
                             (max max-address-length (string-length as)))))]
                [(divider) (format "+~a+" (make-string (+ 2 max-string-length) "-"))])
    #f
    (string-join (cons divider
                       (map (match-lambda
                              [(cons as vs)
                               (format "| ~a |\n~a  0x~a\n"
                                       (~a vs
                                           #:width max-string-length
                                           #:align 'right)
                                       divider
                                       (~a as
                                           #:width max-address-length
                                           #:pad-string "0"
                                           #:align 'right))])
                            as+vses))
                 "")))
