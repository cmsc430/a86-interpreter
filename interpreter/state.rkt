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
;;   instruction-pointer:
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
(struct State (time-tick instruction-pointer labels registers flags memory) #:transparent)

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
                     #f sp)]
       [(registers)
        (hash-set new-registers 'rsp sp)])
    (State 0 ip labels registers new-flags memory)))

;; Given a machine state, takes the next step according to the a86 semantics.
(define (step state)
  (match state
    [(State tick ip labels registers flags memory)
     (match (memory-ref memory ip)
       [(Label _)
        ;; skip
        (State (add1 tick)
               (next-word-aligned-address ip)
               labels registers flags memory)]
       [(Ret)
        ;; ip = Mem.pop()
        (let* ([sp (hash-ref registers 'rsp)]
               [new-ip (memory-ref memory sp)]
               [new-rsp (previous-word-aligned-address sp)]
               [new-registers (hash-set registers 'rsp new-rsp)])
          (State (add1 tick)
                 new-ip labels new-registers flags memory))]
       [(Call dst)
        ;; Mem.push(ip + wordsize)
        ;; ip = dst
        (let* ([return-address (next-word-aligned-address ip)]
               [new-ip (if (register? dst)
                           (hash-ref registers dst)
                           ;; TODO: runtime check valid address
                           dst)]
               [sp (hash-ref registers 'rsp)]
               [new-sp (next-word-aligned-address sp)]
               [new-registers (hash-set registers 'rsp new-sp)])
          (memory-set! memory sp tick return-address)
          (State (add1 tick)
                 new-ip labels registers flags memory))]
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
        (error 'step-Lea)]
       [instruction
        (raise-user-error 'step "unrecognized instruction at address ~a: ~v" ip instruction)])]))
