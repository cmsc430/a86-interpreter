#lang racket

(require "ast.rkt"
         "memory.rkt"
         "registers.rkt"
         "state.rkt"
         "utility.rkt")

(provide step
         interp
         initialize-state
         (all-from-out "ast.rkt"
                       "utility.rkt"))

;; Given a machine state, takes the next step according to the a86 semantics.
(define (step state)
  (match state
    [(State tick ip first-inst last-inst labels registers flags memory)
     (let ([address-from-offset (curry address-from-offset registers)])
       (define (process-argument arg #:as [interpretation '()])
         (cond
           [(and (integer? arg)
                 (or (equal? 'integer interpretation)
                     (member 'integer interpretation)))
            (integer->unsigned arg)]
           [(and (register? arg)
                 (or (equal? 'register interpretation)
                     (member 'register interpretation)))
            (hash-ref registers arg)]
           [(and (offset? arg)
                 (or (equal? 'offset interpretation)
                     (member 'offset interpretation)))
            (address-from-offset registers arg)]
           [(and (valid-address? memory arg)
                 (or (equal? 'address interpretation)
                     (member 'address interpretation)))
            (memory-ref memory arg)]
           [(and (label? arg)
                 (or (equal? 'label interpretation)
                     (member 'label interpretation)))
            (let ([pair (assoc arg labels)])
              (if pair
                  (cdr pair)
                  (raise-user-error 'process-argument "not a valid label: ~a" arg)))]
           [(empty? interpretation)
            (raise-user-error 'process-argument "no interpretation given to process argument")]
           [else
            (raise-user-error 'process-argument
                              "unable to process argument ~v according to interpretation(s) ~a"
                              arg
                              interpretation)]))
       (match (memory-ref memory ip)
         [(Label _)
          ;; skip
          (State (add1 tick)
                 (next-word-aligned-address ip)
                 first-inst last-inst
                 labels registers flags memory)]
         [(Ret)
          ;; ip = Mem.pop()
          (let* ([sp (hash-ref registers 'rsp)]
                 [new-ip (memory-ref memory sp)]
                 [new-rsp (previous-word-aligned-address sp)]
                 [new-registers (hash-set registers 'rsp new-rsp)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers flags memory))]
         [(Call dst)
          ;; Mem.push(ip + wordsize)
          ;; ip = dst
          (let* ([return-address (next-word-aligned-address ip)]
                 [new-ip (if (register? dst)
                             (hash-ref registers dst)
                             (memory-ref memory dst))]
                 [new-sp (next-word-aligned-address (hash-ref registers 'rsp))]
                 [new-registers (hash-set registers 'rsp new-sp)])
            (memory-set! memory new-sp tick return-address)
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers flags memory))]
         [(Mov dst src)
          ;; dst = src
          (let ([argument (process-argument src #:as '(register offset integer))]
                [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               ;; Write to the register.
               (let ([new-registers (hash-set registers dst argument)])
                 (State (add1 tick)
                        new-ip first-inst last-inst labels new-registers flags memory))]
              [(offset? dst)
               ;; Write to memory.
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick argument)
                 (State (add1 tick)
                        new-ip first-inst last-inst labels registers flags memory))]))]
         [(Add dst src)
          ;; dst = dst + src
          ;; % update flags according to [dst + src]
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (hash-ref registers dst)]
                        [(computed-sum new-flags) (bitwise-add base argument)]
                        [(new-ip) (next-word-aligned-address ip)]
                        [(new-registers) (hash-set registers dst computed-sum)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers new-flags memory))]
         [(Sub dst src)
          ;; dst = dst - src
          ;; % update flags according to [dst - src]
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (hash-ref registers dst)]
                        [(computed-diff new-flags) (bitwise-sub base argument)]
                        [(new-ip) (next-word-aligned-address ip)]
                        [(new-registers) (hash-set registers dst computed-diff)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers new-flags memory))]
         [(Cmp a1 a2)
          ;; % update flags according to [a1 - a2]
          (let*-values ([(arg1) (process-argument a1 #:as '(register offset))]
                        [(arg2) (process-argument a2 #:as '(register offset integer))]
                        [(_ new-flags) (bitwise-sub arg1 arg2)]
                        [(new-ip) (next-word-aligned-address ip)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers new-flags memory))]
         [(Jmp t)
          ;; ip = t
          (let ([new-ip (process-argument t #:as '(register label))])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers flags memory))]
         [(Je t)
          ;; if Flags[ZF], ip = t
          (let ([new-ip (if (hash-ref flags 'ZF)
                            (process-argument t #:as '(register label))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers flags memory))]
         [(Jne t)
          ;; if not Flags[ZF], ip = t
          (let ([new-ip (if (not (hash-ref flags 'ZF))
                            (process-argument t #:as '(register label))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers flags memory))]
         [(Jl t)
          ;; if Flags[SF] <> Flags[OF], ip = t
          (let ([new-ip (if (xor (hash-ref flags 'SF)
                                 (hash-ref flags 'OF))
                            (process-argument t #:as '(register label))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers flags memory))]
         [(Jg t)
          ;; if not Flags[ZF] and (Flags[SF] == Flags[OF]), ip = t
          (let ([new-ip (if (and (not (hash-ref flags 'ZF))
                                 (= (hash-ref flags 'SF)
                                    (hash-ref flags 'OF)))
                            (process-argument t #:as '(register label))
                            (next-word-aligned-address ip))])
            (State (add1 tick)
                   new-ip first-inst last-inst labels registers flags memory))]
         [(And dst src)
          ;; dst = dst & src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-and (bitwise-and base argument)]
                 [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-and)])
                 (State (add1 tick)
                        new-ip first-inst last-inst labels new-registers flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-and)
                 (State (add1 tick)
                        new-ip first-inst last-inst labels registers flags memory))]))]
         [(Or dst src)
          ;; dst = dst | src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-or (bitwise-ior base argument)]
                 [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-or)])
                 (State (add1 tick)
                        new-ip first-inst last-inst labels new-registers flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-or)
                 (State (add1 tick)
                        new-ip first-inst last-inst labels registers flags memory))]))]
         [(Xor dst src)
          ;; dst = dst ^ src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-xor (bitwise-xor base argument)]
                 [new-ip (next-word-aligned-address ip)])
            (cond
              [(register? dst)
               (let ([new-registers (hash-set registers dst computed-xor)])
                 (State (add1 tick)
                        new-ip first-inst last-inst labels new-registers flags memory))]
              [(offset? dst)
               (let ([address (address-from-offset dst)])
                 (memory-set! memory address tick computed-xor)
                 (State (add1 tick)
                        new-ip first-inst last-inst labels registers flags memory))]))]
         [(Sal dst i)
          ;; dst = dst << i
          ;;
          ;; NOTE: It is assumed that [i] must be on [0, (word-size-bits) - 1].
          (let* ([base (process-argument dst #:as 'register)]
                 [shifted (mask (arithmetic-shift base i))]
                 [new-registers (hash-set registers dst shifted)]
                 [new-carry (not (= 0 (bitwise-and (arithmetic-shift (arithmetic-shift 1 (word-size-bits)) (- i))
                                                   base)))]
                 [new-overflow (and (= 1 i)
                                    (not (or (and new-carry (not (= 0 (bitwise-and (sign) shifted))))
                                             (and (not new-carry) (= 0 (bitwise-and (sign) shifted))))))]
                 [new-flags (make-new-flags #:overflow new-overflow #:carry new-carry)]
                 [new-ip (next-word-aligned-address ip)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers new-flags memory))]
         [(Sar dst i)
          ;; dst = dst >> i
          ;;
          ;; NOTE: It is assumed that [i] must be on [0, (word-size-bits) - 1].
          (let* ([base (process-argument dst #:as 'register)]
                 [msb? (not (= 0 (bitwise-and (sign) base)))]
                 [shifted (arithmetic-shift base (- i))]
                 [masked (if msb?
                             (bitwise-ior shifted (make-full-mask (- i)))
                             shifted)]
                 [new-registers (hash-set registers dst masked)]
                 [new-carry (not (= 0 (bitwise-and (arithmetic-shift 1 (- i 1))
                                                   base)))]
                 [new-flags (make-new-flags #:carry new-carry)]
                 [new-ip (next-word-aligned-address ip)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers new-flags memory))]
         [(Push a1)
          ;; Mem.push(a1)
          (let* ([base (process-argument a1 #:as '(register integer))]
                 [new-sp (next-word-aligned-address (hash-ref registers 'rsp))]
                 [new-registers (hash-set registers 'rsp new-sp)]
                 [new-ip (next-word-aligned-address ip)])
            (memory-set! memory new-sp tick base)
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers flags memory))]
         [(Pop a1)
          ;; a1 = Mem.pop()
          (let* ([sp (hash-ref registers 'rsp)]
                 [value (memory-ref memory sp)]
                 [new-sp (previous-word-aligned-address sp)]
                 [new-registers (hash-set* registers
                                           'rsp new-sp
                                           a1 value)]
                 [new-ip (next-word-aligned-address ip)])
            (State (add1 tick)
                   new-ip first-inst last-inst labels new-registers flags memory))]
         [(Lea dst x)
          ;; TODO: fix label handling throughout (see syntax definition 'loc'/'l')
          (error 'step-Lea)]
         [instruction
          (raise-user-error 'step "unrecognized instruction at address ~a: ~v" ip instruction)]))]))

(define (interp state [steps 1000] [tracer #f])
  (when tracer
    (if (list? tracer)
        (map (λ (t) ((tracer-func t) t state)) tracer)
        ((tracer-func tracer) tracer state)))
  (cond
    ;; NOTE: We deliberately only check for equality to 0. This allows a user to
    ;; pass a negative number as an argument to cause indefinite interpretation.
    [(zero? steps)
     (cons state 'no-more-steps)]
    [(< (State-instruction-pointer state) (State-last-instruction state))
     (cons state 'no-more-instructions)]
    [else
     (interp (step state) (sub1 steps) tracer)]))

(provide next-instruction)
(define (next-instruction state)
  (let ([ip (State-instruction-pointer state)])
    (and (>= ip (State-last-instruction state))
         (memory-ref (State-memory state) ip))))

(provide (struct-out tracer))
(struct tracer (func [result #:mutable]))

(provide make-register-tracer)
(define (make-register-tracer register)
  (define func (λ (tracer state)
                 (set-tracer-result!
                  tracer
                  (cons (cons (State-time-tick state)
                              (hash-ref (State-registers state) register))
                        (tracer-result tracer)))))
  (tracer func '()))
