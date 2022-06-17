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
     (let ([address-from-offset
            (curry address-from-offset registers)]
           [make-state
            (λ (#:with-ip [new-ip #f]
                #:with-registers [new-registers #f]
                #:with-flags [new-flags #f])
              (State (add1 tick)
                     (or new-ip (next-word-aligned-address ip))
                     first-inst last-inst
                     labels
                     (or new-registers registers)
                     (or new-flags flags)
                     memory))]
           [process-argument
            (λ (arg #:as [interpretation '()])
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
                                   interpretation)]))])
       (match (memory-ref memory ip)
         [(Label _)
          ;; skip
          (make-state)]
         [(Ret)
          ;; ip = Mem.pop()
          (let* ([sp (hash-ref registers 'rsp)]
                 [new-ip (memory-ref memory sp)]
                 [new-rsp (previous-word-aligned-address sp)]
                 [new-registers (hash-set registers 'rsp new-rsp)])
            (make-state #:with-ip new-ip
                        #:with-registers new-registers))]
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
            (make-state #:with-ip new-ip
                        #:with-registers new-registers))]
         [(Mov dst src)
          ;; dst = src
          (let ([argument (process-argument src #:as '(register offset integer))])
            (cond
              [(register? dst)
               ;; Write to the register.
               (make-state #:with-registers (hash-set registers dst argument))]
              [(offset? dst)
               ;; Write to memory.
               (memory-set! memory (address-from-offset dst) tick argument)
               (make-state)]))]
         [(Add dst src)
          ;; dst = dst + src
          ;; % update flags according to [dst + src]
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (hash-ref registers dst)]
                        [(computed-sum new-flags) (bitwise-add base argument)]
                        [(new-registers) (hash-set registers dst computed-sum)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Sub dst src)
          ;; dst = dst - src
          ;; % update flags according to [dst - src]
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (hash-ref registers dst)]
                        [(computed-diff new-flags) (bitwise-sub base argument)]
                        [(new-registers) (hash-set registers dst computed-diff)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Cmp a1 a2)
          ;; % update flags according to [a1 - a2]
          (let*-values ([(arg1) (process-argument a1 #:as '(register offset))]
                        [(arg2) (process-argument a2 #:as '(register offset integer))]
                        [(_ new-flags) (bitwise-sub arg1 arg2)])
            (make-state #:with-flags new-flags))]
         [(Jmp t)
          ;; ip = t
          (make-state #:with-ip (process-argument t #:as '(register label)))]
         [(Je t)
          ;; if Flags[ZF], ip = t
          (make-state #:with-ip (if (hash-ref flags 'ZF)
                                    (process-argument t #:as '(register label))
                                    (next-word-aligned-address ip)))]
         [(Jne t)
          ;; if not Flags[ZF], ip = t
          (make-state #:with-ip (if (not (hash-ref flags 'ZF))
                                    (process-argument t #:as '(register label))
                                    (next-word-aligned-address ip)))]
         [(Jl t)
          ;; if Flags[SF] <> Flags[OF], ip = t
          (make-state #:with-ip (if (xor (hash-ref flags 'SF)
                                         (hash-ref flags 'OF))
                                    (process-argument t #:as '(register label))
                                    (next-word-aligned-address ip)))]
         [(Jg t)
          ;; if not Flags[ZF] and (Flags[SF] == Flags[OF]), ip = t
          (make-state #:with-ip (if (and (not (hash-ref flags 'ZF))
                                         (= (hash-ref flags 'SF)
                                            (hash-ref flags 'OF)))
                                    (process-argument t #:as '(register label))
                                    (next-word-aligned-address ip)))]
         [(And dst src)
          ;; dst = dst & src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-and (bitwise-and base argument)])
            (cond
              [(register? dst)
               (make-state #:with-registers (hash-set registers dst computed-and))]
              [(offset? dst)
               (memory-set! memory (address-from-offset dst) tick computed-and)
               (make-state)]))]
         [(Or dst src)
          ;; dst = dst | src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-or (bitwise-ior base argument)])
            (cond
              [(register? dst)
               (make-state #:with-registers (hash-set registers dst computed-or))]
              [(offset? dst)
               (memory-set! memory (address-from-offset dst) tick computed-or)
               (make-state)]))]
         [(Xor dst src)
          ;; dst = dst ^ src
          (let* ([argument (process-argument src #:as '(register offset integer))]
                 [base (process-argument dst #:as '(register offset))]
                 [computed-xor (bitwise-xor base argument)])
            (cond
              [(register? dst)
               (make-state #:with-registers (hash-set registers dst computed-xor))]
              [(offset? dst)
               (memory-set! memory (address-from-offset dst) tick computed-xor)
               (make-state)]))]
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
                 [new-flags (make-new-flags #:overflow new-overflow #:carry new-carry)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
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
                 [new-flags (make-new-flags #:carry new-carry)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Push a1)
          ;; Mem.push(a1)
          (let* ([base (process-argument a1 #:as '(register integer))]
                 [new-sp (next-word-aligned-address (hash-ref registers 'rsp))]
                 [new-registers (hash-set registers 'rsp new-sp)])
            (memory-set! memory new-sp tick base)
            (make-state #:with-registers new-registers))]
         [(Pop a1)
          ;; a1 = Mem.pop()
          (let* ([sp (hash-ref registers 'rsp)]
                 [value (memory-ref memory sp)]
                 [new-sp (previous-word-aligned-address sp)]
                 [new-registers (hash-set* registers
                                           'rsp new-sp
                                           a1 value)])
            (make-state #:with-registers new-registers))]
         [(Lea dst l)
          ;; dst = Mem.lookup(l)
          (let* ([ea (assoc l labels)])
            (cond
              [(register? dst)
               (make-state #:with-registers (hash-set registers dst ea))]
              [(offset? dst)
               (memory-set! memory (address-from-offset dst) tick ea)
               (make-state)]))]
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
