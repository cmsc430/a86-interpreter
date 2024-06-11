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

         arg-join)

(require "../../registers.rkt"
         "repl-state.rkt"

         (for-syntax (submod "../../utility.rkt" racket/syntax)
                     racket/format
                     racket/list
                     racket/string
                     syntax/parse))

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
                              #:result (apply format
                                              (string-append* (reverse new-form-parts))
                                              (append (reverse new-vs)
                                                      old-vs)))
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
  ;; Previous instruction.
  [I previous-repl-instruction
     (λ (iv) ((current-escape-formatter/~i) iv))]
  ;; Previous value at a memory address.
  [M previous-repl-memory-ref
     (λ (mv _) ((current-escape-formatter/~m) mv))]
  ;; Previous step index.
  [Q previous-repl-emulator-state-index])

(begin-for-syntax
  (define-syntax-class string*
    (pattern string:string)
    (pattern (strings:string ...+)
             #:attr string #`#,(string-join (map syntax-e
                                                 (syntax->list #'(strings ...)))
                                            "\n"))))

(define-syntax (define-show stx)
  (syntax-parse stx
    [(_ (name:id) fmt-str*:string* arg ...)
     #:with fmt-str #'fmt-str*.string
     #'(define (name)
         (displayln (format/repl fmt-str arg ...)))]))

(define-syntax (format-args stx)
  (define-syntax-class arg-string
    (pattern string:string)
    (pattern (strings:string ...+ args ...)))

  (syntax-parse stx
    [(_)
     #'#f]))

(define (arg-join arg-lists [sep "\n"])
  (for/fold ([string-parts  '()]
             [sub-arg-lists '()]
             #:result (cons (string-join (reverse string-parts) sep)
                            (append* (reverse sub-arg-lists))))
            ([arg-list (in-list arg-lists)])
    (match arg-list
      [(? string? string-part)
       (values (cons string-part string-parts)
               sub-arg-lists)]
      [(cons (? string? string-part)
             sub-arg-list)
       (values (cons string-part string-parts)
               (cons sub-arg-list sub-arg-lists))]
      [(or (cons (or #f (? void?)) _)
           #f
           (? void?))
       (values string-parts
               sub-arg-lists)])))
