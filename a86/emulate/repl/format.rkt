#lang racket

(provide format/repl

         current-memory-value-width
         current-instruction-display-count

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

(define current-memory-value-width (make-parameter 18))
(define current-instruction-display-count (make-parameter 11))

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

;; Returns a string formatting the current instruction and its context, and
;; optionally also showing the previous instruction and its context (if
;; available).
(define (format-instructions n-lines
                             [show-prev?        #t]
                             [max-width         30]
                             [prev-indicator "[p]"]
                             [curr-indicator "[c]"])

  ;; Returns an association list mapping addresses to the instructions located
  ;; at those addresses.
  ;;
  ;; An attempt is made to center the instructions around the base address.
  ;; However, if insufficiently many instructions are available, the window is
  ;; shifted. If there are simply not enough instructions available, even with a
  ;; shift, then the necessary quantity of [#f] values are inserted to provide
  ;; the best centering possible.
  ;;
  ;; If [include-address] is given, the window will center between
  ;; [base-address] and [include-address] until the latter is within the window,
  ;; and then the window will expand centered on [base-address].
  ;;
  ;; NOTE: It is assumed that [include-address] is not the same as
  ;; [base-address], is within the natural maximum window range, and refers to a
  ;; readable word in memory that contains an instruction. If any of these
  ;; criteria is not met, [include-address] is treated as though it were [#f].
  (define (retrieve-instructions/centering base-address n
                                           #:base-label    [base-label      'base]
                                           #:including     [include-address #f]
                                           #:include-label [include-label   'include])
    (unless (positive-integer? n)
      (error 'retrieve-instructions/centering "Invalid desired window size: ~v" n))

    ;; Ensure the [include-address] refers to a real instruction-containing
    ;; address within the bounds of our possible window.
    (set! include-address (and include-address
                               (not (= include-address base-address))
                               (or (<= include-address (word-aligned-offset base-address    n))
                                   (>= include-address (word-aligned-offset base-address (- n))))
                               (current-repl-address-readable? include-address)
                               (instruction? (current-repl-memory-ref include-address))
                               include-address))
    ;; Build an association list of address-instruction pairs.
    (let loop ([window-hi-addr     base-address]
               [window-lo-addr     base-address]
               [window-width       1]
               [hi-offset          0]  ;; Offsets are set to [#f] if we can
               [lo-offset          0]  ;; no longer expand in that direction.
               [upper-values       '()]
               [lower-values       '()]
               [include-in-window? #f]
               [include-upwards?   (and include-address (> include-address base-address))])
      ;; Is our window done expanding?
      (if (>= window-width n)
          ;; Yes; return the address-instruction pairs. Recall that the
          ;; [upper-values] and [lower-values] are built from the inside out, so
          ;; we reverse the [lower-values] to go from greatest address to least.
          (append upper-values
                  (list (list base-label (current-repl-memory-ref base-address)))
                  (reverse lower-values))
          ;; No; we must expand.
          (let* ([next-hi-offset (and hi-offset (add1 hi-offset))]
                 [next-lo-offset (and lo-offset (sub1 lo-offset))]
                 [next-hi-addr   (and next-hi-offset (word-aligned-offset base-address next-hi-offset))]
                 [next-lo-addr   (and next-lo-offset (word-aligned-offset base-address next-lo-offset))]
                 [new-hi-addr?   (and next-hi-addr (current-repl-address-readable? next-hi-addr))]
                 [new-lo-addr?   (and next-lo-addr (current-repl-address-readable? next-lo-addr))]
                 [new-hi-value   (and new-hi-addr? (current-repl-memory-ref next-hi-addr))]
                 [new-lo-value   (and new-lo-addr? (current-repl-memory-ref next-lo-addr))]
                 [new-hi-addr?   (instruction? new-hi-value)]
                 [new-lo-addr?   (instruction? new-lo-value)])
            ;; Do we need to keep looking for the [include-address]?
            (if (and include-address (not include-in-window?))
                ;; Yes. Should we be looking upwards?
                (if include-upwards?
                    ;; Yes; take a step that way. NOTE: Safe to assume progress.
                    (let ([include-found? (<= include-address next-hi-addr)])
                      (loop next-hi-addr
                            window-lo-addr
                            (add1 window-width)
                            next-hi-offset
                            lo-offset
                            (cons (if include-found?
                                      (list include-label new-hi-value)
                                      new-hi-value)
                                  upper-values)
                            lower-values
                            include-found?
                            include-upwards?))
                    ;; No; take a step downwards. NOTE: Safe to assume progress.
                    (let ([include-found? (>= include-address next-lo-addr)])
                      (loop window-hi-addr
                            next-lo-addr
                            (add1 window-width)
                            hi-offset
                            next-lo-offset
                            upper-values
                            (cons (if include-found?
                                      (list include-label new-lo-value)
                                      new-lo-value)
                                  lower-values)
                            include-found?
                            include-upwards?)))
                ;; No. Can we take a step upwards?
                (if new-hi-addr?
                    ;; Yes. Do we need to take more than one step, and can we
                    ;; step in both directions?
                    (if (and (>= (- n window-width) 2)
                             new-lo-addr?)
                        ;; Yes; take both steps and continue.
                        (loop next-hi-addr
                              next-lo-addr
                              (+ 2 window-width)
                              next-hi-offset
                              next-lo-offset
                              (cons new-hi-value upper-values)
                              (cons new-lo-value lower-values)
                              include-in-window?
                              include-upwards?)
                        ;; No; take one step upward and continue.
                        (loop next-hi-addr
                              window-lo-addr
                              (add1 window-width)
                              next-hi-offset
                              lo-offset
                              (cons new-hi-value upper-values)
                              lower-values
                              include-in-window?
                              include-upwards?))
                    ;; No. Can we take a step downwards?
                    (if new-lo-addr?
                        ;; Yes; take the step and continue.
                        (loop window-hi-addr
                              next-lo-addr
                              (add1 window-width)
                              #f  ;; We cannot expand upwards.
                              next-lo-offset
                              upper-values
                              (cons new-lo-value lower-values)
                              include-in-window?
                              include-upwards?)
                        ;; No; we must stop expanding. We will have to pad our
                        ;; window with [#f]s appropriately.
                        (let pad-loop ([upper-values upper-values]
                                       [lower-values lower-values]
                                       [window-width window-width])
                          (cond
                            [(>= window-width n)
                             (loop window-hi-addr
                                   window-lo-addr
                                   window-width
                                   hi-offset
                                   lo-offset
                                   upper-values
                                   lower-values
                                   include-in-window?
                                   include-upwards?)]
                            [(> (length upper-values) (length lower-values))
                             (pad-loop upper-values (cons #f lower-values) (add1 window-width))]
                            [else
                             (pad-loop (cons #f upper-values) lower-values (add1 window-width))])))))))))

  ;; Selects instructions that will include both the previous and current
  ;; instruction. If the gap between these is too wide, a break will appear in
  ;; the middle denoted by ['break] instead of an address-instruction pair.
  (define (select-instructions/current-and-previous)
    (let* ([prev-instr-ptr (previous-repl-instruction-pointer)]
           [curr-instr-ptr (current-repl-instruction-pointer)]
           [word-diff (/ (abs (- prev-instr-ptr curr-instr-ptr)) word-size-bytes)])
      ;; Determine whether we need two separate instruction regions.
      (if (> word-diff n-lines)
          ;; Yes, so generate two regions centered on each address.
          (let* ([prev-n (- (quotient n-lines 2)
                            (if (odd? n-lines) 0 1))]
                 [curr-n (quotient n-lines 2)]
                 [prev-instrs (retrieve-instructions/centering prev-instr-ptr prev-n #:base-label 'prev)]
                 [curr-instrs (retrieve-instructions/centering curr-instr-ptr curr-n #:base-label 'curr)])
            (if (> prev-instr-ptr curr-instr-ptr)
                (append prev-instrs
                        (list 'break)
                        curr-instrs)
                (append curr-instrs
                        (list 'break)
                        prev-instrs)))
          ;; No; center on the current address but include the previous.
          (retrieve-instructions/centering curr-instr-ptr n-lines
                                           #:base-label    'curr
                                           #:including     prev-instr-ptr
                                           #:include-label 'prev))))

  ;; Selects [n] instructions that will include the current instruction as
  ;; centered as possible.
  (define (select-instructions/current n)
    (let ([curr-instr-ptr (current-repl-instruction-pointer)])
      (retrieve-instructions/centering curr-instr-ptr n #:base-label 'curr)))

  ;; If we're meant to show the previous instruction AND the previous
  ;; instruction is actually available, do so. Otherwise, show only the current
  ;; instruction.
  (let* ([instrs (if (and show-prev? (previous-repl-instruction-pointer))
                     (select-instructions/current-and-previous)
                     (select-instructions/current n-lines))]
         [indicator-width (max (string-length (or prev-indicator 0))
                               (string-length (or curr-indicator 0)))]
         [instruction-width (- max-width indicator-width)]
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
                                                (if indicator-width 1 0))
                                             #\space)
                                (format-i i))]
                       [v (error 'format-instructions "Unknown value: ~v" v)])])
    (string-join (map format-a+i instrs)
                 "\n")))
