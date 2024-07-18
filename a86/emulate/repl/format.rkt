#lang racket

(provide current-memory-value-width
         current-memory-value-hex-display-width
         current-memory-value-render-display-width
         current-memory-value-separator
         current-instruction-display-count
         current-instruction-display-width

         format/repl

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

         format-memory
         format-instructions)

(require "../../instructions.rkt"
         "../../registers.rkt"
         "../../utility.rkt"
         "exn.rkt"
         "repl-state.rkt"

         (for-syntax (submod "../../utility.rkt" racket/syntax)
                     racket/format
                     racket/list
                     racket/string
                     syntax/parse
                     syntax/parse/lib/function-header))

;; The width of memory values displayed via [format/repl].
(define current-memory-value-width                (make-parameter 18))
;; The width of the hexadecimal representation of a value in a memory display.
(define current-memory-value-hex-display-width    (make-parameter 10))
;; The width of the rendered representation of a value in a memory display. A
;; value of [#f] suppresses outputting rendered values.
(define current-memory-value-render-display-width (make-parameter #f))
;; The string to place between the hexadecimal representation of a value and the
;; rendered representation of a value in a memory display. However, if the
;; [current-memory-value-render-display-width] is [#f], this will not be shown.
(define current-memory-value-separator            (make-parameter "  "))
;; The width of addresses in a memory display.
(define current-memory-address-display-width      (make-parameter 10))
;; The number of instructions to show in an instruction display.
(define current-instruction-display-count         (make-parameter 11))
;; The width of instructions (with indicators) in an instruction display.
(define current-instruction-display-width         (make-parameter 30))

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
       (if (address? mv)
           (~a "0x"
               (~r mv
                   #:base 16
                   #:min-width (- (current-memory-value-width) 2)
                   #:pad-string "0")
               #:width (current-memory-value-width)
               #:align 'right)
           (~a mv
               #:width (current-memory-value-width)
               #:align 'right)))]
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
;;
;; A positive [n] indicates that the [base] address is the lowest address, while
;; a negative [n] indicates that the [base] address is the greatest address.
(define (format-memory base n [direction (if (positive? n) 'up 'down)])
  (unless (not (zero? n))
    (raise-argument-error 'format-memory "non-zero integer?" n))
  (let*-values ([(n) (abs n)]
                [(base-address label)
                 (match base
                   [(? register?) (values (current-repl-register-ref base)
                                          (~a base))]
                   [(? address?)  (values base
                                          #f)])]
                [(as+vses max-v-str-length max-address-length)
                 (for/fold ([as+vses            '()]
                            [max-v-str-length     0]
                            [max-address-length   0])
                           ([i (in-range n)]
                            ;; We want the high addresses to appear at the top,
                            ;; but we're constructing the list backwards, so if
                            ;; we're counting ['up] from the [base-address] then
                            ;; we start with the zero offset and work up, and if
                            ;; we're counting ['down] from the [base-address]
                            ;; then we start with the greatest offset and work
                            ;; down.
                            #:do [(define a (+ base-address
                                               (match direction
                                                 ['up   (* 8 i)]
                                                 ['down (* -8 (- n (add1 i)))])))]
                            #:when (current-repl-address-readable? a))
                   ;; For formatting these boxes, we first format the values in
                   ;; hex, then compare their lengths, and then generate strings
                   ;; for all of them of similar lengths. This makes comparison
                   ;; and printing nice without including more leading zeroes
                   ;; than necessary.
                   (let* ([v (current-repl-memory-ref a)]
                          [v-str (if (a86-value? v)
                                     (~r v #:base 16)
                                     #f)]
                          [a-str (~r a #:base 16)])
                     (values (cons (cons (if (and label (= a base-address))
                                             (~a a-str "  [" label "]")
                                             a-str)
                                         v-str)
                                   as+vses)
                             (max max-v-str-length  (or (and (string? v-str)
                                                              (string-length v-str))
                                                         0))
                             (max max-address-length (string-length a-str)))))]
                ;; Divider is 4 longer than the maximum length of a value string
                ;; because each value will be prefixed with "0x" and surrounded
                ;; by one space on each side.
                [(divider) (format "+~a+" (make-string (+ 4 max-v-str-length) #\-))])
    (string-join (cons divider
                       (map (match-lambda
                              [(cons a-str v-str)
                               (format "\n| ~a |\n~a  0x~a"
                                       (if v-str
                                           (~a "0x"
                                               (~a v-str
                                                   #:pad-string "0"
                                                   #:width max-v-str-length
                                                   #:align 'right))
                                           (make-string (+ 2 max-v-str-length) #\X))
                                       divider
                                       (~a a-str
                                           #:width max-address-length
                                           #:pad-string "0"
                                           #:align 'right))])
                            as+vses))
                 "")))

;; Retrieves the [number-of-values] around [base-address] values from memory,
;; attempting to [align] the [base-address] in a particular way. If an
;; [include-address] is given, then an attempt is made to include that address's
;; value in the emitted list of values, but if [include-address] lies outside
;; the valid range it is omitted without warning. Values are omitted if either
;; [filter-address] returns [#f] when applied to the address or [filter-value]
;; returns [#f] when subsequently applied to the value. The returned list is
;; padded with [#f] values according to the [align]ment to ensure a length of
;; [number-of-values].
;;
;; The in-range values in the returned list have one of the following formats:
;;
;;   * [value]                        --- A normal value.
;;   * (list [base-label] [value])    --- The value at [base-address].
;;   * (list [include-label] [value]) --- The value at [include-address].
;;
;; When the list of values is shorter than [number-of-values], it will be padded
;; according to the [pad-align]. If [pad-align] is ['top], [bottom-pad-value] is
;; added to the bottom of the range of values, ['bottom] will add
;; [top-pad-value] to the top of the range of values, and ['center] will attempt
;; to add padding to both ends to center the [base-address] in the range.
(define (retrieve-memory-values number-of-values
                                base-address
                                #:filter-address   [filter-address   current-repl-address-readable?]
                                #:filter-value     [filter-value     values]
                                #:align            [align            'center]
                                #:base-label       [base-label       'base]
                                #:including        [include-address  #f]
                                #:include-label    [include-label    'include]
                                #:pad-align        [pad-align        'top]
                                #:pad-value        [pad-value        #f]
                                #:top-pad-value    [top-pad-value    pad-value]
                                #:bottom-pad-value [bottom-pad-value pad-value])
  ;; Check assumptions.
  (unless (positive-integer? number-of-values)
    (raise-argument-error 'retrieve-memory-values "positive-integer?" number-of-values))
  (unless (and (address? base-address)
               (current-repl-address-readable? base-address))
    (raise-argument-error 'retrieve-memory-values "readable address?" base-address))
  (unless (and (procedure? filter-address)
               (procedure-arity-includes? filter-address 1))
    (raise-argument-error 'retrieve-memory-values "procedure? with arity including 1" filter-address))
  (unless (and (procedure? filter-value)
               (procedure-arity-includes? filter-value 1))
    (raise-argument-error 'retrieve-memory-values "procedure? with arity including 1" filter-value))
  (unless (memq align '(center top bottom))
    (raise-argument-error 'retrieve-memory-values "one of (center, top, bottom)" align))

  ;; Ensure the [include-address] refers to a real instruction-containing
  ;; address within the bounds of our possible window.
  (set! include-address (and include-address
                             (not (= include-address base-address))
                             (or (<= include-address (word-aligned-offset base-address    number-of-values))
                                 (>= include-address (word-aligned-offset base-address (- number-of-values))))
                             (filter-address include-address)
                             (filter-value (current-repl-memory-ref include-address))
                             include-address))

  ;; Build the window that includes the [include-address], keeping track of the
  ;; [balance] "owed" in the process.
  ;;
  ;; For example, if [align] is ['bottom] but the [include-address] is three
  ;; words below the [base-address], the resulting [balance] will be [-3].
  ;;
  ;; If there is no [include-address], the balance owed is [0].
  ;;
  ;; If [align] is ['top] or ['bottom], a positive balance indicates the
  ;; [include-address] falls within the natural range of the window, i.e., there
  ;; is no need to counteract the shift. Meanwhile, a negative balance indicates
  ;; the window should attempt to scroll back the other direction.
  ;;
  ;; If [align] is ['center], the balance is the offset of the [include-address]
  ;; from the [base-address], where a positive value indicates that the
  ;; [include-address] is greater than the [base-address] and vice versa. In
  ;; this case, the window should always attempt to scroll away from the
  ;; direction of the balance.
  (let*-values ([(balance) (if include-address
                               (/ (case align
                                    [(top)           (- base-address include-address)]
                                    [(bottom center) (- include-address base-address)])
                                  word-size-bytes)
                               0)]
                [(window-hi-addr window-lo-addr hi-offset lo-offset upper-values lower-values)
                 (cond
                   [(or (not include-address)
                        (= base-address include-address))
                    (values base-address
                            base-address
                            0
                            0
                            (list (list 'base (current-repl-memory-ref base-address)))
                            '())]
                   [(> base-address include-address)
                    (let ([offset (- (abs balance))])
                      (values base-address
                              include-address
                              0
                              offset
                              '()
                              (current-repl-memory-ref* base-address (sub1 offset))))]
                   [(< base-address include-address)
                    (let ([offset (abs balance)])
                      (values include-address
                              base-address
                              offset
                              0
                              (reverse (current-repl-memory-ref* base-address (add1 offset)))
                              '()))])]
                [(upper-values lower-values)
                 (let ([correct-value (match-lambda
                                        [(list _ 'unreadable)
                                         (error 'format-memory "invalid range between base-address and include-address")]
                                        [(list a v)
                                         (cond
                                           [(eq? a base-address)
                                            (list base-label v)]
                                           [(eq? a include-address)
                                            (list include-label v)]
                                           [else v])])])
                   (values (if (zero? balance)
                               (list (list base-label (current-repl-memory-ref base-address)))
                               (map correct-value upper-values))
                           (map correct-value lower-values)))])
    ;; Now that our initial window including the [base-address] and
    ;; [include-address] is built, we continue to expand the window until it has
    ;; a length of [number-of-values].
    (let loop ([window-hi-address window-hi-addr]
               [window-lo-address window-lo-addr]
               [window-width      (add1 balance)]
               [balance           balance]
               [hi-offset         hi-offset]
               [lo-offset         lo-offset]
               [upper-values      upper-values]
               [lower-values      lower-values])
      ;; Is our window done expanding?
      (if (>= window-width number-of-values)
          ;; Yes; return the address-value pairs. Recall that the [upper-values]
          ;; and [lower-values] are built from the inside out, so we reverse the
          ;; [lower-values] to ensure our list is ordered from greatest address
          ;; to least.
          (append upper-values (reverse lower-values))
          ;; No; we must expand. Expansion is governed by three factors:
          ;;
          ;;   1. Whether we are able to expand in the desired direction.
          ;;   2. How we want to [align] the [base-address].
          ;;   3. The [balance] owed due to the [include-address].
          ;;
          ;; First, we check in which directions we're able to expand.
          (let* ([next-hi-offset      (and hi-offset (add1 hi-offset))]
                 [next-lo-offset      (and lo-offset (sub1 lo-offset))]
                 [next-hi-address     (and next-hi-offset (word-aligned-offset base-address next-hi-offset))]
                 [next-lo-address     (and next-lo-offset (word-aligned-offset base-address next-lo-offset))]
                 [next-hi-address-ok? (and next-hi-address (filter-address next-hi-address))]
                 [next-lo-address-ok? (and next-lo-address (filter-address next-lo-address))]
                 [next-hi-value       (and next-hi-address-ok? (current-repl-memory-ref next-hi-address))]
                 [next-lo-value       (and next-lo-address-ok? (current-repl-memory-ref next-lo-address))]
                 [next-hi-value-ok?   (and next-hi-value (filter-value next-hi-value))]
                 [next-lo-value-ok?   (and next-lo-value (filter-value next-lo-value))]
                 [expand-upwards      (λ ()
                                        (loop next-hi-address
                                              window-lo-address
                                              (add1 window-width)
                                              (case align
                                                [(top)           (add1 balance)]
                                                [(bottom center) (sub1 balance)])
                                              next-hi-offset
                                              lo-offset
                                              (cons next-hi-value upper-values)
                                              lower-values))]
                 [expand-downwards    (λ ()
                                        (loop window-hi-address
                                              next-lo-address
                                              (add1 window-width)
                                              (case align
                                                [(top)           (add1 balance)]
                                                [(bottom center) (sub1 balance)])
                                              hi-offset
                                              next-lo-offset
                                              upper-values
                                              (cons next-lo-value lower-values)))])
            (cond
              ;; We're able to expand in either direction. Where should we go?
              [(and next-hi-value-ok? next-lo-value-ok?)
               (match (cons align balance)
                 ;; We should expand in both directions simultaneously.
                 [(cons 'center 0)
                  (loop next-hi-address
                        next-lo-address
                        (+ 2 window-width)
                        balance
                        next-hi-offset
                        next-lo-offset
                        (cons next-hi-value upper-values)
                        (cons next-lo-value lower-values))]
                 ;; Either:
                 ;;
                 ;;   - We owe a debt requiring us to expand upwards.
                 ;;   - We are trying to bottom-align and do not owe a debt.
                 [(or (cons (or 'top 'center)      (? negative?))
                      (cons     'bottom       (not (? negative?))))
                  (expand-upwards)]
                 ;; Either:
                 ;;
                 ;;   - We owe a debt requiring us to expand upwards.
                 ;;   - We are trying to top-align and do not owe a debt.
                 ;;   - We are trying to center-align and owe a debt downwards.
                 [(or (cons     'bottom            (? negative?))
                      (cons (or 'top 'center) (not (? negative?))))
                  (expand-downwards)]
                 ;; Otherwise, we don't know what's happening.
                 [_
                  (error 'retrieve-memory-values
                         "unknown align/balance combination: ~v, ~v"
                         align
                         balance)])]
              ;; We can only expand upwards.
              [next-hi-value-ok? (expand-upwards)]
              ;; We can only expand downwards.
              [next-lo-value-ok? (expand-downwards)]
              ;; We can't expand at all, so we must pad our list(s).
              [else
               (let ([total-pad-length (- number-of-values window-width)])
                 (case pad-align
                   ;; Add padding only at the bottom.
                   [(top)
                    (loop window-hi-address
                          window-lo-address
                          number-of-values
                          balance
                          hi-offset
                          lo-offset
                          upper-values
                          (append (make-list total-pad-length bottom-pad-value) lower-values))]
                   ;; Add padding only at the top.
                   [(bottom)
                    (loop window-hi-address
                          window-lo-address
                          number-of-values
                          balance
                          hi-offset
                          lo-offset
                          (append (make-list total-pad-length top-pad-value) upper-values)
                          lower-values)]
                   ;; Add padding around the retrieved values. We first attempt
                   ;; to re-center the [base-address] by correcting for the
                   ;; difference in the offsets, and then padding is added in
                   ;; equal measure to both sides, with preference given to the
                   ;; lower values if an odd amount must be added.
                   [(center)
                    (let*-values ([(align-pad-length) (- hi-offset (abs lo-offset))]
                                  [(split-pad-length) (- total-pad-length (abs align-pad-length))]
                                  [(split-pad-length extra-pad-length)
                                   (if (positive? split-pad-length)
                                       (quotient/remainder split-pad-length 2)
                                       (values 0 0))]
                                  [(upper-pad-length) (+ split-pad-length
                                                         (if (negative? align-pad-length)
                                                             (min total-pad-length (abs align-pad-length))
                                                             split-pad-length))]
                                  [(lower-pad-length) (+ split-pad-length
                                                         extra-pad-length
                                                         (if (positive? align-pad-length)
                                                             (min total-pad-length align-pad-length)
                                                             split-pad-length))])
                      (loop window-hi-address
                            window-lo-address
                            number-of-values
                            balance
                            hi-offset
                            lo-offset
                            (append (make-list upper-pad-length top-pad-value)    upper-values)
                            (append (make-list lower-pad-length bottom-pad-value) lower-values)))]))]))))))

;; Returns a string formatting the current instruction and its context, and
;; optionally also showing the previous instruction and its context (if
;; available).
(define (format-instructions number-of-lines
                             [show-prev?     #t]
                             [max-width      (current-instruction-display-width)]
                             [prev-indicator "[p]"]
                             [curr-indicator "[c]"])

  ;; Selects instructions that will include both the previous and current
  ;; instruction. If the gap between these is too wide, a break will appear in
  ;; the middle denoted by ['break] instead of an address-instruction pair.
  (define (select-instructions/current-and-previous)
    (let* ([prev-instr-ptr (previous-repl-instruction-pointer)]
           [curr-instr-ptr (current-repl-instruction-pointer)]
           [word-diff (/ (abs (- prev-instr-ptr curr-instr-ptr)) word-size-bytes)])
      ;; Determine whether we need two separate instruction regions.
      (if (> word-diff number-of-lines)
          ;; Yes, so generate two regions centered on each address.
          (let* ([prev-n (- (quotient number-of-lines 2)
                            (if (odd? number-of-lines) 0 1))]
                 [curr-n (quotient number-of-lines 2)]
                 [prev-instrs (retrieve-memory-values prev-n prev-instr-ptr
                                                      #:filter-value instruction?
                                                      #:base-label   'prev)]
                 [curr-instrs (retrieve-memory-values curr-n curr-instr-ptr
                                                      #:filter-value instruction?
                                                      #:base-label   'curr)])
            (if (> prev-instr-ptr curr-instr-ptr)
                (append prev-instrs
                        (list 'break)
                        curr-instrs)
                (append curr-instrs
                        (list 'break)
                        prev-instrs)))
          ;; No; center on the current address but include the previous.
          (retrieve-memory-values number-of-lines curr-instr-ptr
                                  #:filter-value  instruction?
                                  #:base-label    'curr
                                  #:including     prev-instr-ptr
                                  #:include-label 'prev))))

  ;; Selects [n] instructions that will include the current instruction as
  ;; centered as possible.
  (define (select-instructions/current n)
    (let ([curr-instr-ptr (current-repl-instruction-pointer)])
      (retrieve-memory-values n curr-instr-ptr
                              #:filter-value instruction?
                              #:base-label   'curr)))

  ;; If we're meant to show the previous instruction AND the previous
  ;; instruction is actually available, do so. Otherwise, show only the current
  ;; instruction.
  (let* ([instrs (if (and show-prev? (previous-repl-instruction-pointer))
                     (select-instructions/current-and-previous)
                     (select-instructions/current number-of-lines))]
         [indicator-width (max (string-length (or prev-indicator 0))
                               (string-length (or curr-indicator 0)))]
         [gutter-width (if (zero? indicator-width) 0 1)]
         [instruction-width (- max-width indicator-width gutter-width)]
         [format-i (λ (i) (~v i
                              #:max-width instruction-width
                              #:limit-marker "...)"))]
         [format-a+i (match-lambda
                       [(list (and indicator (or 'curr 'prev)) i)
                        (let ([indicator (match indicator
                                           ['curr curr-indicator]
                                           ['prev prev-indicator])])
                          (format "~a~a~a"
                                  (~a (or indicator "")
                                      #:width indicator-width
                                      #:align 'right)
                                  (if (zero? indicator-width) "" " ")
                                  (format-i i)))]
                       ['break (make-string max-width #\-)]
                       [(? instruction? i)
                        (format "~a~a"
                                (make-string (+ indicator-width
                                                gutter-width)
                                             #\space)
                                (format-i i))]
                       [#f (make-string max-width #\space)]
                       [v (error 'format-instructions "Unknown value: ~v" v)])])
    (string-join (map format-a+i instrs)
                 "\n")))
