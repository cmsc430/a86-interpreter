#lang racket


(provide (struct-out StepState)
         end-of-program-signal
         step/manual)

(require "../ast.rkt"
         "../debug.rkt"
         "../registers.rkt"
         "../utility.rkt"

         "memory.rkt"
         "runtime.rkt"
         "state.rkt"

         (rename-in "memory.rkt" [memory-ref original-memory-ref])

         (for-syntax syntax/parse
                     racket/syntax))

;; Provides a form for defining new binary instructions, e.g., [Add] and [Sub].
(define-syntax (define-binary-instruction stx)
  (syntax-parse stx
    [(_ (name arg1 arg2)
        (~seq #:base-computation base-computation)
        (~optional (~seq #:result-name result-name)
                   #:defaults ([result-name #'value]))
        (~optional (~seq #:overflow-computation overflow-computation))
        (~optional (~seq #:sign-computation     sign-computation))
        (~optional (~seq #:zero-computation     zero-computation))
        (~optional (~seq #:carry-computation    carry-computation)))
     (with-syntax ([func-name          (format-id #'name #:source #'name "a86:~a"         (syntax-e #'name))]
                   [base-result        (format-id #'result-name          "base-~a"        (syntax-e #'result-name))]
                   [masked-result      (format-id #'result-name          "masked-~a"      (syntax-e #'result-name))]
                   [masked-result-sign (format-id #'result-name          "masked-~a-sign" (syntax-e #'result-name))]
                   [arg1-sign          (format-id #'arg1                 "~a-sign"        (syntax-e #'arg1))]
                   [arg2-sign          (format-id #'arg1                 "~a-sign"        (syntax-e #'arg2))])
       #'(define (func-name arg1 arg2)
           (unless (unsigned-in-bounds? arg1)
             (raise-user-error 'func-name "first argument not within word size bounds: ~a" arg1))
           (unless (unsigned-in-bounds? arg2)
             (raise-user-error 'func-name "second argument not within word size bounds: ~a" arg2))
           (let* ([arg1-sign (bitwise-and sign-mask arg1)]
                  [arg2-sign (bitwise-and sign-mask arg2)]
                  [base-result base-computation]
                  [masked-result (truncate-integer/unsigned base-result)]
                  [masked-result-sign (bitwise-and sign-mask masked-result)]
                  [set-overflow? (~? (~@ overflow-computation)
                                     (~@ #f))]
                  [set-sign? (~? (~@ sign-computation)
                                 (~@ (not (= 0 masked-result-sign))))]
                  [set-zero? (~? (~@ zero-computation)
                                 (~@ (= 0 masked-result)))]
                  [set-carry? (~? (~@ carry-computation)
                                  (~@ (not (= 0 (bitwise-and base-result
                                                             (arithmetic-shift 1 word-size-bits))))))]
                  [flags (make-flags #:overflow set-overflow?
                                     #:sign set-sign?
                                     #:zero set-zero?
                                     #:carry set-carry?)])
             (values masked-result flags))))]))

;; Given two integers, calculates their sum. Returns the sum along with a new
;; set of flags.
(define-binary-instruction (add a1 a2)
  #:base-computation (+ a1 a2)
  #:result-name sum
  #:overflow-computation (and (= a1-sign a2-sign)
                              (not (= a1-sign masked-sum-sign))))

;; Given two integers, calculates their difference. Returns the difference along
;; with a new set of flags.
(define-binary-instruction (sub a1 a2)
  #:base-computation (- a1 a2)
  #:result-name diff
  #:overflow-computation (or (and (= 0 a1-sign)
                                  (not (= 0 a2-sign))
                                  (not (= 0 masked-diff-sign)))
                             (and (not (= 0 a1-sign))
                                  (= 0 a2-sign)
                                  (= 0 masked-diff-sign))))

;; Given two integers, calculates their bitwise conjunction. Returns the result
;; along with a new set of flags.
(define-binary-instruction (and a1 a2)
  #:base-computation (bitwise-and a1 a2)
  #:carry-computation #f)

;; Given two integers, calculates their bitwise inclusive disjunction. Returns
;; the result along with a new set of flags.
(define-binary-instruction (ior a1 a2)
  #:base-computation (bitwise-ior a1 a2)
  #:carry-computation #f)

;; Given two integers, calculates their bitwise exclusive disjunction. Returns
;; the result along with a new set of flags.
(define-binary-instruction (xor a1 a2)
  #:base-computation (bitwise-xor a1 a2)
  #:carry-computation #f)

;; A value that signals the end of computation. If the input program attempts to
;; [Ret] while this value is in the position of the expected return address, the
;; execution will halt.
(define end-of-program-signal 'end)

;; Executes a single step in the interpreter, producing a new [StepState?].
;;
;; The interpreter works by reading the next instruction from memory according
;; to the [ip] field of the given [StepState?]. Memory is modified in-place,
;; but registers and flags are set anew with each step to produce a new
;; [StepState?].
(define (step/manual step-state memory labels->addresses)
  (match step-state
    [(StepState time-tick ip flags registers _)
     (let* ([current-instruction (original-memory-ref memory ip)]
            [_ (debug "step ~a: ~v\t[~a]" time-tick current-instruction (format-word ip 'hex))]
            ;; Memory transactions are tracked automatically by the [memory-ref]
            ;; and [memory-set!] functions (defined below) and then inserted
            ;; into the next [StepState].
            [transactions (list)]
            ;; A convenience for calling [memory-ref] that also tracks the read
            ;; transaction.
            [memory-ref (λ (address)
                          (let ([value (memory-ref memory address)])
                            (set! transactions
                                  (cons (make-read-transaction address value word-size-bytes)
                                        transactions))
                            value))]
            ;; A convenience for calling [memory-set!] that also tracks the
            ;; write transaction.
            [memory-set! (λ (address value [byte-count word-size-bytes])
                           (begin0
                               (memory-set! memory address time-tick value byte-count)
                             (set! transactions
                                   (cons (make-write-transaction address value byte-count)
                                         transactions))))]
            ;; Converts an [Offset] into an address.
            [address-from-offset (match-lambda [(Offset reg off) (+ off (register-ref registers reg))])]
            ;; Checks if an instruction address is valid.
            [assert-ip-valid!
             (λ (new-ip)
               (match-let ([(list lo-address hi-address)
                            (address-range memory text)])
                 (unless (and (>= new-ip lo-address)
                              (<= new-ip hi-address))
                   (raise-user-error 'segfault "invalid instruction pointer: ~v" new-ip))))]
            ;; Called as the last step for every instruction's implementation.
            ;; The time tick is incremented, and other values are set as needed.
            [make-step-state
             (λ (#:with-ip        [new-ip        #f]
                 #:with-flags     [new-flags     #f]
                 #:with-registers [new-registers #f])
               (StepState (add1 time-tick)
                          (or new-ip (lesser-word-aligned-address ip))
                          (or new-flags flags)
                          (or new-registers registers)
                          transactions))]
            ;; For instructions that require processing arguments in specialized
            ;; ways. For example, a [Mov] instruction can process its second
            ;; argument (the "source") as a register, a memory offset, or an
            ;; immediate value.
            ;;
            ;; NOTE: Though most of this function could exist outside the parent
            ;; [step] function, the ['label] and ['external-function] variants
            ;; require knowledge of the available labels and external functions.
            ;; TODO: If we instead parameterize labels and the runtime, this
            ;; could be implemented externally, which might offer a small
            ;; performance improvement? I'm not too worried about it right now,
            ;; but it could be nice to do at some point.
            [process-argument
             (λ (arg #:as interpretation)
               (let ([interpretation-matches?
                      (λ (type)
                        (or (equal? type interpretation)
                            (and (list? interpretation)
                                 (member type interpretation))))])
                 (cond
                   [(and (interpretation-matches? 'integer)
                         (integer? arg))
                    (truncate-integer/unsigned arg)]
                   [(and (interpretation-matches? 'register)
                         (register? arg))
                    (register-ref registers arg)]
                   [(and (interpretation-matches? 'offset)
                         (offset? arg))
                    (memory-ref (address-from-offset arg))]
                   [(and (interpretation-matches? 'address)
                         (a86-value? arg))
                    arg]
                   [(and (interpretation-matches? 'label)
                         (symbol? arg))
                    (cond
                      [(hash-ref labels->addresses arg)
                       => (λ (p) p)]
                      [else
                       (raise-user-error 'step "not a valid label: ~a" arg)])]
                   [(and (interpretation-matches? 'external-function)
                         (symbol? arg))
                    (let ([external-function (runtime-ref arg)])
                      (if external-function
                          external-function
                          (raise-user-error 'step "not a valid external function: ~a" arg)))]
                   [(and (list? interpretation)
                         (empty? interpretation))
                    (raise-user-error 'step "no interpretation given to process argument")]
                   [else
                    (raise-user-error 'step
                                      "unable to process argument ~v according to interpretation(s) ~a"
                                      arg
                                      interpretation)])))]
            ;; Performs arithmetic operations.
            [do-arith (λ (dst src op)
                        (let*-values ([(arg) (process-argument src #:as '(register offset integer))]
                                      [(base) (register-ref registers dst)]
                                      [(computed-result new-flags) (op base arg)]
                                      [(new-registers) (register-set/truncate registers dst computed-result)])
                          (make-step-state #:with-flags new-flags
                                           #:with-registers new-registers)))]
            ;; Conditionally performs a jump to a target address.
            [jump-with-condition
             (λ (target condition)
               (let ([jump? (or (eq? condition #t)
                                (condition flags))])
                 (debug "    jumping to ~v: ~v" target jump?)
                 (if jump?
                     (let ([new-ip (process-argument target #:as '(register label))])
                       (assert-ip-valid! new-ip)
                       (make-step-state #:with-ip new-ip))
                     (make-step-state))))]
            ;; Conditional moves a source into a destination.
            [move-with-condition
             (λ (dst src condition)
               (let ([move? (or (eq? condition #t)
                                (condition flags))]
                     ;; NOTE: Conditional move instructions read from the source
                     ;; operand before evaluating the condition, so we must do
                     ;; the same.
                     [val (process-argument src #:as '(register offset integer))])
                 (debug "    moving ~v to ~v: ~v" src dst move?)
                 (debug "      src val: ~v" val)
                 (if move?
                     (cond
                       [(register? dst)
                        (make-step-state #:with-registers (register-set/truncate registers dst val))]
                       [(offset? dst)
                        (let ([addr (address-from-offset dst)]
                              ;; NOTE: If moving into something that doesn't
                              ;; fit full words, it should be addressed here.
                              [byte-count (cond
                                            [(register/32-bit? src) 4]
                                            [else 8])])
                          (debug "      dst addr: ~v" addr)
                          (memory-set! addr val byte-count)
                          (make-step-state))])
                     (make-step-state))))])
       ;; TODO: Add more [debug] stuff throughout.
       (match current-instruction
         [(Push arg)
          ;; stack.push(arg)
          ;; rsp -= word-size-bytes
          (let* ([base (process-argument arg #:as '(register integer))]
                 [new-sp (lesser-word-aligned-address (register-ref registers 'rsp))]
                 [new-registers (register-set registers 'rsp new-sp)])
            (memory-set! new-sp base)
            (make-step-state #:with-registers new-registers))]
         [(Pop arg)
          ;; rsp += word-size-bytes
          ;; arg = stack.pop()
          (let* ([sp (register-ref registers 'rsp)]
                 [value (memory-ref sp)]
                 [new-sp (greater-word-aligned-address sp)]
                 [new-registers (register-set*/truncate registers
                                                        'rsp new-sp
                                                        arg value)])
            (make-step-state #:with-registers new-registers))]
         [(Pushf)
          ;; stack.push(FLAGS)
          ;; rsp -= word-size-byte
          (let* ([word (flags->word flags)]
                 [new-sp (lesser-word-aligned-address (register-ref registers 'rsp))]
                 [new-registers (register-set registers 'rsp new-sp)])
            (memory-set! new-sp word)
            (make-step-state #:with-registers new-registers))]
         [(Popf)
          ;; rsp += word-size-bytes
          ;; FLAGS = stack.pop()
          (let* ([sp (register-ref registers 'rsp)]
                 [word (memory-ref sp)]
                 [new-sp (greater-word-aligned-address sp)]
                 [new-registers (register-set/truncate registers 'rsp new-sp)]
                 [new-flags (word->flags word)])
            (make-step-state #:with-registers new-registers
                             #:with-flags new-flags))]

         [(Ret)
          ;; ip = stack.pop()
          (let* ([sp (register-ref registers 'rsp)]
                 [new-ip (memory-ref sp)]
                 [new-rsp (greater-word-aligned-address sp)]
                 [new-registers (register-set registers 'rsp new-rsp)])
            ;; TODO: Test this out.
            (if (eq? new-ip end-of-program-signal)
                (begin (debug "    terminating program")
                       step-state)
                (begin (debug "    returning to: ~a" (format-word new-ip 'hex))
                       (make-step-state #:with-ip new-ip
                                        #:with-registers new-registers))))]
         [(Call dst)
          ;; % The handling of a Call depends upon the argument.
          (if (runtime-has-func? dst)
              ;; If the target of the Call is a label corresponding to an
              ;; external function in the runtime, we call that function.
              (let* ([func (process-argument dst #:as 'external-function)]
                     [sp (register-ref registers 'rsp)]
                     [result (func flags registers memory sp)])
                (cond
                  [(void? result)
                   (make-step-state)]
                  [(not (integer? result))
                   (raise-user-error 'step-Call
                                     "result of call to external function '~a' not void? or integer?: ~v"
                                     dst
                                     result)]
                  [(not (<= (integer-length result) word-size-bits))
                   (raise-user-error 'step-Call
                                     "integer result of call to external function '~a' too wide: ~v"
                                     dst
                                     result)]
                  [else
                   (make-step-state #:with-registers (register-set registers 'rax result))]))
              ;; stack.push(ip + wordsize)
              ;; ip = dst
              (let* ([return-address (lesser-word-aligned-address ip)]
                     [new-ip (process-argument dst #:as '(register label))]
                     [new-sp (lesser-word-aligned-address (register-ref registers 'rsp))]
                     [new-registers (register-set registers 'rsp new-sp)])
                (memory-set! new-sp return-address)
                (debug "  wrote return address ~a to stack address ~a"
                       (format-word return-address 'hex)
                       (format-word new-sp 'hex))
                (debug "  retrieving address: ~a" (format-word
                                                   (original-memory-ref memory new-sp)
                                                   'hex))
                (make-step-state #:with-ip new-ip
                                 #:with-registers new-registers)))]

         [(Not x)
          ;; x = !x
          (let* ([base (process-argument x #:as 'register)]
                 [negated (bitwise-not base)]
                 [new-registers (register-set/truncate registers x negated)])
            (make-step-state #:with-registers new-registers))]
         [(Add dst src)
          ;; dst = dst + src
          (do-arith dst src a86:add)]
         [(Sub dst src)
          ;; dst = dst - src
          (do-arith dst src a86:sub)]
         [(And dst src)
          ;; dst = dst & src
          (do-arith dst src a86:and)]
         [(Or dst src)
          ;; dst = dst | src
          (do-arith dst src a86:ior)]
         [(Xor dst src)
          ;; dst = dst ^ src
          (do-arith dst src a86:xor)]
         [(Cmp a1 a2)
          ;; % update flags according to [a1 - a2].
          (let*-values ([(arg1) (process-argument a1 #:as '(register offset))]
                        [(arg2) (process-argument a2 #:as '(register offset integer))]
                        [(_ new-flags) (a86:sub arg1 arg2)])
            (make-step-state #:with-flags new-flags))]
         [(Sal dst i)
          ;; dst = dst << i
          ;;
          ;; NOTE: It is assumed that [i] must be on [0, word-size-bits - 1].
          (let* ([base (process-argument dst #:as 'register)]
                 [shifted (truncate-integer/unsigned (arithmetic-shift base i))]
                 [new-registers (register-set/truncate registers dst shifted)]
                 [set-carry? (not (= 0 (bitwise-and (arithmetic-shift (arithmetic-shift 1 word-size-bits) (- i))
                                                    base)))]
                 [set-overflow? (and (= 1 i)
                                     (not (or (and      set-carry?  (not (= 0 (bitwise-and sign-mask shifted))))
                                              (and (not set-carry?)      (= 0 (bitwise-and sign-mask shifted))))))]
                 [new-flags (make-flags #:overflow set-overflow?
                                        #:carry set-carry?)])
            (make-step-state #:with-flags new-flags
                             #:with-registers new-registers))]
         [(Sar dst i)
          ;; dst = dst >> i
          ;;
          ;; NOTE: It is assumed that [i] must be on [0, word-size-bits - 1].
          (let* ([base (process-argument dst #:as 'register)]
                 [msb? (not (= 0 (bitwise-and sign-mask base)))]
                 [shifted (arithmetic-shift base (- i))]
                 [masked (if msb?
                             (bitwise-ior shifted (make-mask (- i)))
                             shifted)]
                 [new-registers (register-set/truncate registers dst masked)]
                 [set-carry? (not (= 0 (bitwise-and (arithmetic-shift 1 (- i 1))
                                                    base)))]
                 [new-flags (make-flags #:carry set-carry?)])
            (make-step-state #:with-flags new-flags
                             #:with-registers new-registers))]

         [(Jmp t)
          ;; % unconditionally set the instruction pointer to [t].
          (jump-with-condition t #t)]
         [(Je t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 = a2].
          (jump-with-condition t flags-e?)]
         [(Jne t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 <> a2].
          (jump-with-condition t flags-ne?)]
         [(Jl t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 < a2].
          (jump-with-condition t flags-l?)]
         [(Jle t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 <= a2].
          (jump-with-condition t flags-le?)]
         [(Jg t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 > a2].
          (jump-with-condition t flags-g?)]
         [(Jge t)
          ;; % jump to [t] when [Cmp a1 a2] indicates [a1 >= a2].
          (jump-with-condition t flags-ge?)]
         [(Jo t)
          ;; % jump to [t] when [Cmp a1 a2] sets the overflow flag.
          (jump-with-condition t flags-o?)]
         [(Jno t)
          ;; % jump to [t] when [Cmp a1 a2] unsets the overflow flag.
          (jump-with-condition t flags-no?)]
         [(Jc t)
          ;; % jump to [t] when [Cmp a1 a2] sets the carry flag.
          (jump-with-condition t flags-c?)]
         [(Jnc t)
          ;; % jump to [t] when [Cmp a1 a2] unsets the carry flag.
          (jump-with-condition t flags-nc?)]

         [(Mov dst src)
          ;; dst = src
          (move-with-condition dst src #t)]
         [(Cmove dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 = a2].
          ( move-with-condition dst src flags-e?)]
         [(Cmovne dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 <> a2].
          (move-with-condition dst src flags-ne?)]
         [(Cmovl dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 < a2].
          (move-with-condition dst src flags-l?)]
         [(Cmovle dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 <= a2].
          (move-with-condition dst src flags-le?)]
         [(Cmovg dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 > a2].
          (move-with-condition dst src flags-g?)]
         [(Cmovge dst src)
          ;; dst = src  % when [Cmp a1 a2] indicates [a1 >= a2].
          (move-with-condition dst src flags-ge?)]
         [(Cmovo dst src)
          ;; dst = src  % when [Cmp a1 a2] sets the overflow flag.
          (move-with-condition dst src flags-o?)]
         [(Cmovno dst src)
          ;; dst = src  % when [Cmp a1 a2] unsets the overflow flag.
          (move-with-condition dst src flags-no?)]
         [(Cmovc dst src)
          ;; dst = src  % when [Cmp a1 a2] sets the carry flag.
          (move-with-condition dst src flags-c?)]
         [(Cmovnc dst src)
          ;; dst = src  % when [Cmp a1 a2] unsets the carry flag.
          (move-with-condition dst src flags-nc?)]

         [(Lea dst l)
          (let ([ea (hash-ref labels->addresses l)])
            (cond
              [(register? dst)
               ;; dst = address-of(l)
               (make-step-state #:with-registers (register-set/truncate registers dst ea))]
              [(offset? dst)
               ;; memory[dst] = address-of(l)
               (memory-set! (address-from-offset dst) ea)
               (make-step-state)]))]
         [_
          (raise-user-error 'step
                            "unrecognized instruction ~v at address ~a"
                            current-instruction
                            ip)]))]))
