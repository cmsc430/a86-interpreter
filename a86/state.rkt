#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt"
         "utility.rkt")

(provide (struct-out State)
         initialize-state)

;; A representation of the current state of the machine. The state includes:
;;
;;   first-instruction:
;;       The address of the first instruction, usually the highest address in
;;       memory. Immutable.
;;
;;   last-instruction:
;;       The address of the last instruction, which is used to initialize the
;;       stack pointer register 'rsp. Immutable.
;;
;;   runtime:
;;       A hash mapping external function names to implementations. Immutable.
;;
;;   labels:
;;       An association list mapping label names to addresses. Immutable.
;;
;;   time-tick:
;;       An integer representing how many instructions have been evaluated.
;;
;;   instruction-pointer:
;;       The address of the next instruction to execute.
;;
;;   flags:
;;       A hash mapping flag names to their values.
;;
;;   registers:
;;       A hash mapping register names to their values.
;;
;;   memory:
;;       A [Memory] object from ["memory.rkt"], representing the current stack.
;;
;; TODO: Remove transparency?
;; TODO: Revise implementations of recording devices for consistency? Currently,
;; the registers and flags are meant to be updated functionally while the
;; memory is stateful.
(struct State (first-instruction
               last-instruction
               runtime
               labels
               globals
               externs
               time-tick
               instruction-pointer
               flags
               registers
               memory)
  #:transparent)

;; Converts an external (native Racket) function into one that accepts arguments
;; from the registers and stack. This obeys typical x86 calling conventions,
;; namely: the six registers 'rdi 'rsi 'rdx 'rcx 'r8 and 'r9 are used in that
;; order for passing the first six arguments to the function, and additional
;; arguments are passed in reverse order on the stack.
(define (convert-external-function func)
  (match (procedure-arity func)
    [0 (λ (rs m sp) (func))]
    [1 (λ (rs m sp) (func (hash-ref rs 'rdi)))]
    [2 (λ (rs m sp) (func (hash-ref rs 'rdi) (hash-ref rs 'rsi)))]
    [3 (λ (rs m sp) (func (hash-ref rs 'rdi) (hash-ref rs 'rsi) (hash-ref rs 'rdx)))]
    [4 (λ (rs m sp) (func (hash-ref rs 'rdi) (hash-ref rs 'rsi) (hash-ref rs 'rdx) (hash-ref rs 'rcx)))]
    [5 (λ (rs m sp) (func (hash-ref rs 'rdi) (hash-ref rs 'rsi) (hash-ref rs 'rdx) (hash-ref rs 'rcx) (hash-ref rs 'r8)))]
    [6 (λ (rs m sp) (func (hash-ref rs 'rdi) (hash-ref rs 'rsi) (hash-ref rs 'rdx) (hash-ref rs 'rcx) (hash-ref rs 'r8) (hash-ref rs 'r9)))]
    [(? integer? argc)
     (λ (rs m sp)
       (let ([reg-args (list (hash-ref rs 'rdi) (hash-ref rs 'rsi)
                             (hash-ref rs 'rdx) (hash-ref rs 'rcx)
                             (hash-ref rs 'r8) (hash-ref rs 'r9))]
             [mem-args (for/fold ([result (list)]
                                  [sp sp]
                                  #:result (reverse result))
                                 ([_ (range (- argc 6))])
                         ;; Read value from memory and increment stack pointer.
                         (values (cons (memory-ref m sp) result)
                                 (previous-word-aligned-address sp)))])
         (apply func (append reg-args mem-args))))]
    [(? list?)
     (raise-user-error
      'initialize-state
      "a86 currently does not support runtime functions with optional arguments; please wrap your function")]))

;; Given a [Program], initializes the machine state.
(define (initialize-state program
                          #:runtime [runtime (hash)])
  (let*-values
      ([(entry-point) (Program-entry-point program)]
       [(ip sp memory)
        (initialize-memory (let ([main (gensym 'main)])
                             (sequence
                              (Jmp main)
                              (Program-instructions program)
                              (Label main)
                              (Call entry-point))))]
       [(labels) (memory-fold (λ (labels addr value)
                                (match value
                                  [(or (Label l)
                                       (Extern l))
                                   (cons (cons l addr) labels)]
                                  [_ labels]))
                              (list)
                              memory
                              #f (next-word-aligned-address sp))]
       ;; [(for/hash )]  ;; TODO: new for/fold/memory?
       [(globals) (Program-globals program)]
       [(externs) (Program-externs program)]
       [(registers)
        (hash-set new-registers 'rsp sp)]
       [(runtime)
        (for/hash ([(key func) (in-hash runtime)])
          (values key (convert-external-function func)))])
    (State ip sp runtime labels globals externs 0 ip new-flags registers memory)))
