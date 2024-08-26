#lang racket

(provide show-modes
         show/simple
         show/compact
         show/complete

         with-repl-show-mode)

(require "../memory.rkt"
         "format.rkt"
         "repl-state.rkt"
         (submod "repl-state.rkt" private)

         (for-syntax racket/syntax
                     syntax/parse))

;; [show] modes:
;;
;; - simple
;; - compact
;; - expanded
;;
;; NOTE: [show] prioritizes showing the last-executed instruction.
;;
;;
;; Simple Mode:
;;
;; The least amount of useful information available. Each step shows:
;;
;;   [flags] [reg r/w indicator] instruction
;;
;; The [flags] displays which flags were modified by the [instruction]. A
;; possible output might look like [OC  ], indicating the Overflow and Carry
;; flags were modified.
;;
;; The [reg r/w indicator] notifies you if a register was read from or written
;; to. A possible output might look like [rax w], indicating the [rax] register
;; was written to. This indicator is not displayed if no registers were used. If
;; multiple registers were used, they will all be shown, separated by commas.
;; Note that the instruction pointer register is not indicated here.
;;
;; Lastly, [instruction] shows the a86 instruction that was executed.
;;
;;
;; Compact Mode:
;;
;; More information than Simple mode, but less than Expanded. Each step shows:
;;
;;   [prev-flags] prev-instruction
;;   [curr-flags] curr-instruction
;;
;;   [reg r/w value]*
;;
;;   [mem r/w value]*
;;
;; [prev-flags] and [curr-flags] display the settings of the flags, with changes
;; highlighted.
;;
;; [prev-instruction] and [curr-instruction] show the a86 instruction that was
;; last executed and the instruction that is set to be executed next.
;;
;; The [reg r/w value]* shows any registers that were read from or written to,
;; with the corresponding value displayed. The [mem r/w value]* is the same, but
;; for memory addresses.
;;
;;
;; Expanded Mode:
;;
;; The most information. Each step shows:
;;
;;   * the previous flags and instruction
;;   * the current flags and instruction
;;   * all registers, their values, and indications of whether the last
;;     instruction read from or wrote to them
;;   * the memory around the current stack pointer
;;   * the heap (?)
;;
;;
;; In addition to the default modes, [show] can be given arguments to display
;; specific information.
;;
;;   * display specific register(s)
;;   * display all registers
;;   * display specific memory address(es)
;;   * display range of memory addresses
;;   * display ranges of memory addresses
;;   * display flags
;;
;; All settings can be configured so that they will be used each time the
;; program steps.

(define show-modes
  `([simple   ,show/simple]
    [compact  ,show/compact]
    [complete ,show/complete]))

(define-show (show/simple) "~q. [~f*] ~i")

(define-show (show/compact)
  ;; When we're not at the beginning state, show the previous
  ;; state as well.
  [(positive-integer? (current-repl-emulator-state-index))
   "~Q. [~F*] ~I"]
  ;; Always show the current state.
  "~q. [~f*] ~i"
  ;; If there is any output, show it all.
  [(positive-integer? (file-position* (current-repl-output-port)))
   "output: \"~>>\""]
  ;; If there is an input, show it.
  [(current-repl-input-port)
   "input:  \"~<<<\""]
  ;; If any input has been consumed, show how much.
  [(positive-integer? (file-position* (current-repl-input-port)))
   "pos:     ~<<^"])

(define (map-pad proc def0 lst0 . args)
  (let loop ([defs (list def0)]
             [lsts (list lst0)]
             [args args])
    (match args
      ;; Take the next default value and list, then loop.
      [(list* defn lstn args)
       (loop (cons defn defs)
             (cons lstn lsts)
             args)]
      ;; Done accumulating default values and lists of arguments. Map time!
      [(list)
       (let ([defs (reverse defs)]
             [lsts (reverse lsts)])
         (if (andmap null? lsts)
             '()
             (let loop ([ress '()]
                        [lsts lsts])
               (if (ormap values lsts)
                   ;; At least one of the lists still has elements.
                   (for/fold ([args     '()]
                              [new-lsts '()]
                              #:result (loop (cons (apply proc (reverse args))
                                                   ress)
                                             (reverse new-lsts)))
                             ([def defs]
                              [lst lsts])
                     (match lst
                       [#f           (values (cons def args) (cons #f  new-lsts))]
                       [(cons v '()) (values (cons v   args) (cons #f  new-lsts))]
                       [(cons v lst) (values (cons v   args) (cons lst new-lsts))]))
                   ;; No list has elements; conclude iteration.
                   (reverse ress)))))]
      ;; Inconsistent number of arguments.
      [_ (error 'map-pad "expected equal number of default arguments and list arguments")])))

(define (show/complete)
  (let* ([is-str (format-instructions (current-instruction-display-count))]
         [is-lines (string-split is-str "\n")]
         #;[max-is-line-len (apply max (map string-length is-lines))]
         [max-is-line-len (current-instruction-display-width)]
         [rsp-base (current-repl-register-ref 'rsp)]
         [st-str (format-memory (quotient (current-instruction-display-count) 2)
                                rsp-base
                                #:label "rsp"
                                #:including (address-range-hi (current-repl-memory) stack)
                                #:include-label "stack top")]
         [st-lines (string-split st-str "\n")]
         ;; TODO: It'd be nice to generalize this and make it more configurable.
         #;[max-st-line-len (apply max (map string-length st-lines))]
         [merged-lines
          (map-pad (λ (is-line st-line)
                     (string-append (~a is-line #:width max-is-line-len)
                                    "  "
                                    (~a st-line)))
                   ""
                   is-lines
                   ""
                   st-lines)])
    (displayln
     (string-join
      (append merged-lines
              (list ""
                    (format/repl "Step: ~q")
                    (format/repl "Flags: ~f*"))
              (if (positive-integer? (file-position* (current-repl-output-port)))
                  (list (format/repl "Output: \"~>>\""))
                  (list))
              (if (current-repl-input-port)
                  (list (format/repl "Input:  \"~<<<\""))
                  (list))
              (if (positive-integer? (file-position* (current-repl-input-port)))
                  (list (format/repl "         ~<<^"))
                  (list))
              (list ""))
      "\n"))))

(define (set-repl-state-show-mode! repl-state show-mode)
  (let ([show-proc (assoc show-mode show-modes)])
    (unless show-proc
      (raise-user-error 'repl "not a valid show mode: ~s" show-mode))
    (set-repl-state-show-proc! repl-state show-proc)))

(define-syntax (with-repl-show-mode stx)
  (syntax-parse stx
    [(_ new-mode body ...+)
     #'(let ([old-mode-proc (current-repl-show-proc)])
         (dynamic-wind
           ;; NOTE: Uses symbolic mode name.
           (λ () (set-repl-state-show-mode! (current-repl-state) new-mode))
           (λ () body ...)
           ;; NOTE: Uses proc directly.
           (λ () (set-repl-state-show-proc! (current-repl-state) old-mode-proc))))]))
