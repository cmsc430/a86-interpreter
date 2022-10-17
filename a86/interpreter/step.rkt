#lang racket

(require "../ast.rkt"
         "memory.rkt"
         "registers.rkt"
         "state.rkt"
         "utility.rkt")

(provide step
         multi-step
         initialize-state
         (all-from-out "../ast.rkt"
                       "utility.rkt"))

;; Given a machine state, takes the next step according to the a86 semantics.
(define (step state)
  (match state
    [(State first-inst last-inst runtime labels globals externs tick ip flags registers memory)
     (let* ([current-instruction (memory-ref memory ip)]
            [address-from-offset (curry address-from-offset registers)]
            [make-state
             (λ (#:with-ip [new-ip #f]
                 #:with-registers [new-registers #f]
                 #:with-flags [new-flags #f])
               (State first-inst last-inst runtime labels globals externs
                      (add1 tick)
                      (or new-ip (next-word-aligned-address ip))
                      (or new-flags flags)
                      (or new-registers registers)
                      memory))]
            [process-argument
             (λ (arg #:as [interpretation '()])
               (let* ([op-name (get-instruction-name current-instruction)]
                      [func-name (string->symbol (string-append "process-argument-"
                                                                (symbol->string op-name)))])
                 (cond
                   [(and (or (equal? 'integer interpretation)
                             (member 'integer interpretation))
                         (integer? arg))
                    (integer->unsigned arg)]
                   [(and (or (equal? 'register interpretation)
                             (member 'register interpretation))
                         (register? arg))
                    (hash-ref registers arg)]
                   [(and (or (equal? 'offset interpretation)
                             (member 'offset interpretation))
                         (offset? arg))
                    (address-from-offset registers arg)]
                   [(and (or (equal? 'address interpretation)
                             (member 'address interpretation))
                         (valid-address? memory arg))
                    (memory-ref memory arg)]
                   [(and (or (equal? 'label interpretation)
                             (member 'label interpretation))
                         (symbol? arg))
                    (let ([pair (assoc arg labels)])
                      (if pair
                          (cdr pair)
                          (raise-user-error func-name "not a valid label: ~a" arg)))]
                   [(and (or (equal? 'external-function interpretation)
                             (member 'external-function interpretation))
                         (symbol? arg))
                    (let ([external-function (hash-ref runtime arg #:failure-result #f)])
                      (if external-function
                          external-function
                          (raise-user-error func-name "not a valid external function: ~a" arg)))]
                   [(empty? interpretation)
                    (raise-user-error func-name "no interpretation given to process argument")]
                   [else
                    (raise-user-error func-name
                                      "unable to process argument ~v according to interpretation(s) ~a"
                                      arg
                                      interpretation)])))])
       (match current-instruction
         [(or (? label-type?)
              (? Comment?))
          ;; do nothing; invariants associated with the label types should be
          ;; checked either in Program construction or State initialization.
          (make-state)]
         [(Ret)
          ;; ip = Mem.pop()
          (let* ([sp (hash-ref registers 'rsp)]
                 [new-ip (memory-ref memory sp)]
                 [new-rsp (previous-word-aligned-address sp)]
                 [new-registers (hash-set registers 'rsp new-rsp)])
            (make-state #:with-ip new-ip
                        #:with-registers new-registers))]
         [(Call (? (curry hash-has-key? runtime) external-function))
          ;; call the external function
          (let* ([func (process-argument external-function #:as 'external-function)]
                 [sp (hash-ref registers 'rsp)]
                 [result (func registers memory sp)])
            (cond
              [(void? result)
               (make-state)]
              [(not (integer? result))
               (raise-user-error 'step-Call
                                 "result of external function '~a' not #<void> or an integer; got ~v"
                                 external-function
                                 result)]
              [(not (<= (integer-length result) (word-size-bits)))
               (raise-user-error 'step-Call
                                 "integer result of external function '~a' too large; got ~v"
                                 external-function
                                 result)]
              [else
               (let ([new-registers (hash-set registers 'rax result)])
                 (make-state #:with-registers new-registers))]))]
         [(Call dst)
          ;; Mem.push(ip + wordsize)
          ;; ip = dst
          (let* ([return-address (next-word-aligned-address ip)]
                 [new-ip (process-argument dst #:as '(register label))]
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
                        [(computed-sum new-flags) (a86:add base argument)]
                        [(new-registers) (hash-set registers dst computed-sum)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Sub dst src)
          ;; dst = dst - src
          ;; % update flags according to [dst - src]
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (hash-ref registers dst)]
                        [(computed-diff new-flags) (a86:sub base argument)]
                        [(new-registers) (hash-set registers dst computed-diff)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(And dst src)
          ;; dst = dst & src
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (process-argument dst #:as '(register offset))]
                        [(computed-and new-flags) (a86:and base argument)]
                        [(new-registers) (hash-set registers dst computed-and)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Or dst src)
          ;; dst = dst | src
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (process-argument dst #:as '(register offset))]
                        [(computed-ior new-flags) (a86:ior base argument)]
                        [(new-registers) (hash-set registers dst computed-ior)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Xor dst src)
          ;; dst = dst ^ src
          (let*-values ([(argument) (process-argument src #:as '(register offset integer))]
                        [(base) (process-argument dst #:as '(register offset))]
                        [(computed-xor new-flags) (a86:xor base argument)]
                        [(new-registers) (hash-set registers dst computed-xor)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Sal dst i)
          ;; dst = dst << i
          ;;
          ;; NOTE: It is assumed that [i] must be on [0, (word-size-bits) - 1].
          (let* ([base (process-argument dst #:as 'register)]
                 [shifted (mask (arithmetic-shift base i))]
                 [new-registers (hash-set registers dst shifted)]
                 [set-carry? (not (= 0 (bitwise-and (arithmetic-shift (arithmetic-shift 1 (word-size-bits)) (- i))
                                                    base)))]
                 [set-overflow? (and (= 1 i)
                                     (not (or (and      set-carry?  (not (= 0 (bitwise-and (sign) shifted))))
                                              (and (not set-carry?)      (= 0 (bitwise-and (sign) shifted))))))]
                 [new-flags (make-new-flags #:overflow set-overflow? #:carry set-carry?)])
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
                 [set-carry? (not (= 0 (bitwise-and (arithmetic-shift 1 (- i 1))
                                                    base)))]
                 [new-flags (make-new-flags #:carry set-carry?)])
            (make-state #:with-registers new-registers
                        #:with-flags new-flags))]
         [(Cmp a1 a2)
          ;; % update flags according to [a1 - a2]
          (let*-values ([(arg1) (process-argument a1 #:as '(register offset))]
                        [(arg2) (process-argument a2 #:as '(register offset integer))]
                        [(_ new-flags) (a86:sub arg1 arg2)])
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

(define (multi-step state [steps 1000] [tracer #f])
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
     (multi-step (step state) (sub1 steps) tracer)]))

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
