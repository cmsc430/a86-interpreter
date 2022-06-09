#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt"
         "state.rkt"
         "utility.rkt")

(provide step
         initialize-state
         (all-from-out "ast.rkt"
                       "utility.rkt"))

;; Given a machine state, takes the next step according to the a86 semantics.
(define (step state)
  (match state
    [(State tick ip labels registers flags memory)
     (let* ([address-from-offset (curry address-from-offset registers)]
            [process-argument (λ (x) (cond [(register? x)
                                            (hash-ref registers x)]
                                           [(offset? x)
                                            (address-from-offset registers x)]
                                           [else x]))])
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
                             (memory-ref memory dst))]
                 [sp (hash-ref registers 'rsp)]
                 [new-sp (next-word-aligned-address sp)]
                 [new-registers (hash-set registers 'rsp new-sp)])
            (memory-set! memory sp tick return-address)
            (State (add1 tick)
                   new-ip labels new-registers flags memory))]
         [(Mov dst src)
          ;; dst = src
          (let ([argument (integer->unsigned (process-argument src))]
                [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               ;; Write to the register.
               (let ([new-registers (hash-set registers dst argument)])
                 (State (add1 tick)
                        new-ip labels new-registers flags memory))]
              [(offset? dst)
               ;; Write to memory.
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick argument)
                 (State (add1 tick)
                        new-ip labels registers flags memory))]))]
         [(Add dst src)
          ;; dst = dst + src
          ;; % update flags according to [dst + src]
          (let*-values ([(argument) (integer->unsigned (process-argument src))]
                        [(base) (integer->unsigned (process-argument dst))]
                        [(computed-sum new-flags) (bitwise-add base argument)]
                        [(new-ip) (next-word-aligned-address ip)])
            ;; FIXME: Should Add allow offsets?
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-sum)])
                 (State (add1 tick)
                        new-ip labels new-registers new-flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-sum)
                 (State (add1 tick)
                        new-ip labels registers new-flags memory))]))]
         [(Sub dst src)
          ;; dst = dst - src
          ;; % update flags according to [dst - src]
          (let*-values ([(argument) (integer->unsigned (process-argument src))]
                        [(base) (integer->unsigned (process-argument dst))]
                        [(computed-diff new-flags) (bitwise-sub base argument)]
                        [(new-ip) (next-word-aligned-address ip)])
            ;; FIXME: Should Sub allow offsets?
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-diff)])
                 (State (add1 tick)
                        new-ip labels new-registers new-flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-diff)
                 (State (add1 tick)
                        new-ip labels registers new-flags memory))]))]
         [(Cmp a1 a2)
          ;; % update flags according to [a1 - a2]
          (let*-values ([(arg1) (integer->unsigned (process-argument a1))]
                        [(arg2) (integer->unsigned (process-argument a2))]
                        [(_ new-flags) (bitwise-sub arg1 arg2)]
                        [(new-ip) (next-word-aligned-address ip)])
            (State (add1 tick)
                   new-ip labels registers new-flags memory))]
         [(Jmp t)
          ;; ip = t
          (let ([new-ip (if (register? t)
                            (hash-ref registers t)
                            (memory-ref memory t))])
            (State (add1 tick)
                   new-ip labels registers flags memory))]
         [(Je t)
          ;; if Flags[ZF], ip = t
          (let ([new-ip (if (hash-ref flags 'ZF)
                            (if (register? t)
                                (hash-ref registers t)
                                (memory-ref memory t))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip labels registers flags memory))]
         [(Jne t)
          ;; if not Flags[ZF], ip = t
          (let ([new-ip (if (not (hash-ref flags 'ZF))
                            (if (register? t)
                                (hash-ref registers t)
                                (memory-ref memory t))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip labels registers flags memory))]
         [(Jl t)
          ;; if Flags[SF] <> Flags[OF], ip = t
          (let ([new-ip (if (xor (hash-ref flags 'SF)
                                 (hash-ref flags 'OF))
                            (if (register? t)
                                (hash-ref registers t)
                                (memory-ref memory t))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip labels registers flags memory))]
         [(Jg t)
          ;; if not Flags[ZF] and (Flags[SF] == Flags[OF]), ip = t
          (let ([new-ip (if (and (not (hash-ref flags 'ZF))
                                 (= (hash-ref flags 'SF)
                                    (hash-ref flags 'OF)))
                            (if (register? t)
                                (hash-ref registers t)
                                (memory-ref memory t))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip labels registers flags memory))]
         [(And dst src)
          ;; dst = dst & src
          (let* ([argument (integer->unsigned (process-argument src))]
                 [base (integer->unsigned (process-argument dst))]
                 [computed-and (bitwise-and base argument)]
                 [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-and)])
                 (State (add1 tick)
                        new-ip labels new-registers flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-and)
                 (State (add1 tick)
                        new-ip labels registers flags memory))]))]
         [(Or dst src)
          ;; dst = dst | src
          (let* ([argument (integer->unsigned (process-argument src))]
                 [base (integer->unsigned (process-argument dst))]
                 [computed-or (bitwise-ior base argument)]
                 [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-or)])
                 (State (add1 tick)
                        new-ip labels new-registers flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-or)
                 (State (add1 tick)
                        new-ip labels registers flags memory))]))]
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
          (raise-user-error 'step "unrecognized instruction at address ~a: ~v" ip instruction)]))]))
