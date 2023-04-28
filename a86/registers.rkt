#lang racket

(provide registers/64-bit
         registers/32-bit
         machine-register-names
         all-register-names

         register?
         register/64-bit?
         register/32-bit?

         register-ref
         register-set
         register-set*
         register-set/truncate
         register-set*/truncate

         fresh-registers

         make-flags
         flag-names
         fresh-flags
         flag?

         flags-o?
         flags-s?
         flags-z?
         flags-c?
         flags-no?
         flags-ns?
         flags-nz?
         flags-nc?

         flags-e?
         flags-ne?
         flags-l?
         flags-le?
         flags-g?
         flags-ge?

         debug-flags
         trace-registers
         debug-registers)

(require "debug.rkt"
         "utility.rkt"

         (only-in racket/private/hash
                  paired-fold)

         (for-syntax syntax/parse))

;; We define a set of accessible 64-bit and 32-bit registers for use in the
;; machine.
(define-values (registers/64-bit registers/32-bit)
  (values
   '(rax rbx rcx rdx rbp rsp rsi rdi r8  r9  r10  r11  r12  r13  r14  r15)
   '(eax ebx ecx edx ebp esp esi edi r8d r9d r10d r11d r12d r13d r14d r15d)))

;; The machine's "actual" registers are only the 64-bit ones. The smaller
;; registers are just masks over the low bits in the larger registers.
(define machine-register-names registers/64-bit)
;; Sometimes it is useful to have a list of all the register names.
(define all-register-names (append registers/64-bit registers/32-bit))

;; A [register?] is a symbol that is in one of the above lists. We further
;; distinguish these by bit-width, for convenience.
(define (register? r)
  (member r all-register-names))
(define (register/64-bit? r)
  (member r registers/64-bit))
(define (register/32-bit? r)
  (member r registers/32-bit))

;; Internally, we have to be able to map the smaller registers to their larger
;; counterparts to perform lookups.
;;
;; NOTE: This makes the assumption that the definitions above exactly align in
;; their order of declaration, such that the first 32-bit register name is meant
;; to correspond to the lower 32 bits of the first 64-bit register.
(define register-assoc (map cons registers/32-bit registers/64-bit))
(define (register/32-bit->register/64-bit r)
  (cdr (assoc r register-assoc)))

;; We hide the hash-map behind a struct. This ensures invariants are maintained.
;; It also allows us to define our own [register-ref] and [register-set]
;; functions that can handle lookup of low-bit registers.
(struct register-hash (registers))

;; Retrieves the value stored in the indicated register. If the indicated
;; register is width-restricted, a mask is applied to the resulting value.
(define (register-ref registers r)
  (unless (register? r)
    (error 'register-ref "not a register: ~v" r))
  (cond
    [(register/64-bit? r)
     (hash-ref (register-hash-registers registers) r)]
    [(register/32-bit? r)
     (let ([v (hash-ref (register-hash-registers registers)
                        (register/32-bit->register/64-bit r))])
       (bitwise-and v max-unsigned/32-bit))]))

;; Creates a new [register-hash?] where the value stored at register [r] is
;; replaced with [v].
;;
;; NOTE: The value must be a positive integer on the unsigned interval
;; corresponding to the indicated register's bit-width.
(define (register-set registers r v)
  (unless (register? r)
    (error 'register-set "cannot set register ~v to ~v; not a register" r v))
  (unless (a86-value? v)
    (error 'register-set "cannot set register ~v to ~v; not an a86 value" r v))
  (cond
    [(register/64-bit? r)
     (register-hash (hash-set (register-hash-registers registers) r v))]
    [(register/32-bit? r)
     (unless (a86-value/32-bit? v)
       (error 'register-set "cannot set 32-bit register ~v to ~v; not a 32-bit a86 value" r v))
     (register-hash (hash-set (register-hash-registers registers)
                              (register/32-bit->register/64-bit r)
                              (bitwise-and v max-unsigned/32-bit)))]))

;; Performs multiple consecutive [register-set] operations from left to right.
(define (register-set* registers . pairs)
  (paired-fold 'register-set* pairs registers register-set))

;; Like [register-set], but truncates the value itself instead of assuming the
;; value is already truncated to the appropriate bit-width.
(define (register-set/truncate registers r v)
  (unless (register? r)
    (error 'register-set/truncate "not a register: ~v" r))
  (register-set registers
                r
                (cond
                  [(register/64-bit? r)
                   (bitwise-and v max-unsigned)]
                  [(register/32-bit? r)
                   (bitwise-and v max-unsigned/32-bit)])))

;; Like [register-set*], but with [register-set/truncate].
(define (register-set*/truncate registers . pairs)
  (paired-fold 'register-set*/truncate pairs registers register-set/truncate))

;; Creates a fresh [register-hash?] with empty registers.
(define fresh-registers
  (register-hash (apply hash (flatten (map (λ (r) (list r 0)) machine-register-names)))))

;; The flags are represented as a hashmap from flag names to values. Flags
;; should only be set to either [#t] or [#f]. Flags are initialized to [#f].
(define (make-flags #:overflow [overflow #f]
                    #:sign     [sign     #f]
                    #:zero     [zero     #f]
                    #:carry    [carry    #f])
  (hash 'OF overflow
        'SF sign
        'ZF zero
        'CF carry))
(define flag-names
  '(OF SF ZF CF))
(define fresh-flags (make-flags))
(define (flag? f)
  (member f flag-names))

(define (flags-o? flags) (hash-ref flags 'OF))
(define (flags-s? flags) (hash-ref flags 'SF))
(define (flags-z? flags) (hash-ref flags 'ZF))
(define (flags-c? flags) (hash-ref flags 'CF))
(define (flags-no? flags) (not (hash-ref flags 'OF)))
(define (flags-ns? flags) (not (hash-ref flags 'SF)))
(define (flags-nz? flags) (not (hash-ref flags 'ZF)))
(define (flags-nc? flags) (not (hash-ref flags 'CF)))

(define flags-e? flags-z?)
(define flags-ne? flags-nz?)
(define (flags-l? flags) (xor (hash-ref flags 'SF)
                              (hash-ref flags 'OF)))
(define (flags-le? flags) (or (hash-ref flags 'ZF)
                              (xor (hash-ref flags 'SF)
                                   (hash-ref flags 'OF))))
(define (flags-g? flags) (and (flags-ne? flags)
                              (eq? (hash-ref flags 'SF)
                                   (hash-ref flags 'OF))))
(define (flags-ge? flags) (eq? (hash-ref flags 'SF)
                               (hash-ref flags 'OF)))

(define (debug-flags flags)
  (when debug-on?
    (debug "  OF: ~v  SF: ~v  ZF: ~v  CF: ~v"
           (hash-ref flags 'OF)
           (hash-ref flags 'SF)
           (hash-ref flags 'ZF)
           (hash-ref flags 'CF))))

;; A list of registers to output whenever [debug-registers] is called, if debug
;; mode is enabled.
;;
;; TODO: This (and [debug-registers]) should perhaps be moved elsewhere.
(define trace-registers (make-parameter (list)))

(define (debug-registers registers)
  (when debug-on?
    (for ([reg (trace-registers)])
      (debug "  ~v: ~a" reg (format-word (register-ref registers reg) 'hex)))))
