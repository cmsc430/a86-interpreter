#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt"
         "utility.rkt")

(provide (struct-out State)
         initialize-state)

;; A representation of the current state of the machine. The state includes:
;;
;;   time-tick:
;;       An integer representing how many instructions have been evaluated.
;;
;;   instruction-pointer:
;;       The address of the next instruction to execute.
;;
;;   first-instruction:
;;       The address of the first instruction, usually the highest address in
;;       memory.
;;
;;   last-instruction:
;;       The address of the last instruction, which is used to initialize the
;;       stack pointer register 'rsp.
;;
;;   labels:
;;       An association list mapping label names to addresses.
;;
;;   registers:
;;       A hash mapping register names to their values.
;;
;;   flags:
;;       A hash mapping flag names to their values.
;;
;;   memory:
;;       A [Memory] object from ["memory.rkt"], representing the current stack.
;;
;; TODO: Remove transparency?
;; TODO: Revise implementations of recording devices for consistency? Currently,
;; the registers and flags are meant to be updated functionally while the
;; memory is stateful.
(struct State (time-tick
               instruction-pointer
               first-instruction
               last-instruction
               labels
               registers
               flags
               memory)
  #:transparent)

;; Given a [Program], initializes the machine state.
(define (initialize-state program)
  (let*-values
      ([(ip sp memory)
        (initialize-memory (Program-instructions program))]
       [(labels)
        (memory-fold (λ (labels addr value)
                       (if (Label? value)
                           (cons (cons (Label-x value) addr) labels)
                           labels))
                     (list)
                     memory
                     #f (next-word-aligned-address sp))]
       [(registers)
        (hash-set new-registers 'rsp sp)])
    (State 0 ip ip sp labels registers new-flags memory)))
