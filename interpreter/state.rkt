#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt")

(provide initialize-state
         step)

;; A representation of the current state of the machine. The state includes:
;;
;;   time-tick:
;;       An integer representing how many instructions have been evaluated.
;;
;;   program-counter:
;;       The address of the next instruction to execute.
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
(struct State (time-tick program-counter labels registers flags memory) #:transparent)

;; Given a [Program], initializes the machine state.
(define (initialize-state program)
  (let*-values
      ([(pc sp memory)
        (initialize-memory (Program-instructions program))]
       [(labels)
        (memory-fold (λ (labels addr value)
                       (if (Label? value)
                           (cons (cons (Label-x value) addr) labels)
                           labels))
                     (list)
                     memory
                     #f sp)]
       [(registers)
        (hash-set new-registers 'rsp sp)])
    (State 0 pc labels registers new-flags memory)))

;; Given a machine state, takes the next step according to the a86 semantics.
(define (step state)
  (match state
    [(State tick pc labels registers flags memory)
     (match (memory-ref memory pc)
       [(Label _)
        (State (add1 tick)
               (next-word-aligned-address pc)
               labels registers flags memory)]
       [(Ret)
        (error 'step-Ret)]
       [(Call x)
        (error 'step-Call)]
       [(Mov dst src)
        (error 'step-Mov)]
       [(Add dst src)
        (error 'step-Add)]
       [(Sub dst src)
        (error 'step-Sub)]
       [(Cmp a1 a2)
        (error 'step-Cmp)]
       [(Jmp t)
        (error 'step-Jmp)]
       [(Je t)
        (error 'step-Je)]
       [(Jne t)
        (error 'step-Jne)]
       [(Jl t)
        (error 'step-Jl)]
       [(Jg t)
        (error 'step-Jg)]
       [(And dst src)
        (error 'step-And)]
       [(Or dst src)
        (error 'step-Or)]
       [(Xor dst src)
        (error 'step-Xor)]
       [(Sal dst i)
        (error 'step-Sal)]
       [(Sar dst i)
        (error 'step-Sar)]
       [(Push a1)
        (error 'step-Push)]
       [(Pop a1)
        (error 'step-Pop)]
       [(Lea dst x)
        (error 'step-Lea)])]))
