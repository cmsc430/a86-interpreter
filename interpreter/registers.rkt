#lang racket

(provide (all-defined-out))

;; The registers are represented as a hashmap from register names to values.
;; Note that the special register reference [eax] is not included; this must be
;; translated by the machine. Registers are initialized to [0].
(define register-names
  '(rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
(define new-registers
  (apply hash (flatten (map (λ (r) (list r 0)) register-names))))
(define (register? r)
  (or (member r register-names)
      (equal? r 'eax)))

;; The flags are represented as a hashmap from flag names to values. Flags
;; should only be set to either [#t] or [#f]. Flags are initialized to [#f].
(define flag-names
  '(OF SF ZF CF))
(define new-flags
  (apply hash (flatten (map (λ (f) (list f #f)) flag-names))))
(define (flag? f)
  (member f flag-names))
