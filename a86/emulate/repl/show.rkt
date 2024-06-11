#lang racket

(provide show/simple
         show/compact)

(require "format.rkt"
         "repl-state.rkt"

         #;(for-syntax (submod "../../utility.rkt" racket/syntax)
                     racket/format
                     racket/list
                     racket/string
                     syntax/parse))

(struct ShowConfig (mode))

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

(define-show (show/simple) "~q. [~f*] ~i")
#;(define-show (show/compact) ["~Q. [~F*] ~I"
                             "~q. [~f*] ~i"
                             "output: \"~>>\""
                             "input:  \"~<<\""])

(define (show/compact)
  (displayln
   (apply format/repl
          (arg-join (list (when (positive-integer? (current-repl-emulator-state-index))
                            "~Q. [~F*] ~I")
                          (list "~q. [~f*] ~i")
                          (when (positive-integer? (file-position* (current-repl-output-port)))
                            "output: \"~>>\"")
                          (when (current-repl-input-port)
                            (list "input:  \"~<<<\""
                                  "pos:     ~a" (~a "^"
                                                    #:width (add1 (file-position* (current-repl-input-port)))
                                                    #:pad-string "~"
                                                    #:align 'right)) ))))))



;; FIXME -----------------------------------------------------------------------
#;(format/repl "overflow? ~f\nrax: ~r\nlast instruction: ~I" 'OF 'rax)
