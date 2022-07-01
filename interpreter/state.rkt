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
;;   runtime:
;;       A hash mapping external function names to implementations.
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
               memory
               runtime)
  #:transparent)

;; Converts an external (native Racket) function into one that accepts arguments
;; from the registers and stack. This obeys typical x86 calling conventions,
;; namely: the six registers 'rdi 'rsi 'rdx 'rcx 'r8 and 'r9 are used in that
;; order for passing the first six arguments to the function, and additional
;; arguments are passed in reverse order on the stack.
(define (convert-external-function func)
  (let* ([register-order '(rdi rsi rdx rcx r8 r9)]
         [regc (length register-order)]
         [argc (procedure-arity func)])
    (cond
      [(integer? argc)
       (λ (registers memory sp)
         (let ([args (for/list ([argn (range argc)])
                       (if (< argn regc)
                           ;; Take the argument from the appropriate register.
                           (hash-ref registers (list-ref register-order argn))
                           ;; Take the argument from the stack.
                           ;; TODO: implement this!
                           (error 'initialize-state "external functions needing more than 6 arguments are not yet implemented")))])
           (apply func args)))]
      [(list? argc)
       (raise-user-error
        'initialize-state
        "a86 currently does not support runtime functions with optional arguments; please wrap your function")])))

;; Given a [Program], initializes the machine state.
(define (initialize-state program [runtime (hash)])
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
        (hash-set new-registers 'rsp sp)]
       [(runtime)
        (for/hash ([(key func) (in-hash runtime)])
          (values key (convert-external-function func)))])
    (State 0 ip ip sp labels registers new-flags memory runtime)))
