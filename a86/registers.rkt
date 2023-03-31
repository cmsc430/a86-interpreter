#lang racket

(provide register-names
         fresh-registers
         register?

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
         "utility.rkt")

;; The registers are represented as a hashmap from register names to values.
;; Note that the special register reference [eax] is not included; this must be
;; translated by the machine. Registers are initialized to [0].
(define register-names
  '(rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
(define fresh-registers
  (apply hash (flatten (map (λ (r) (list r 0)) register-names))))
(define (register? r)
  (or (member r register-names)
      (equal? r 'eax)))

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

(define trace-registers (make-parameter (list)))

(define (debug-registers registers)
  (when debug-on?
    (for ([reg (trace-registers)])
      (debug "  ~v: ~a" reg (format-word (hash-ref registers reg) 'hex)))))
