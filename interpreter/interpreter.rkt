#lang racket

(require racket/format)

(provide (all-defined-out))

;; The size of words, given in bits.
(define word-size-bits 64)
(define word-size-bytes (/ word-size-bits 8))

(define (format-word value [mode 'binary])
  (match mode
    [(or 'binary 'b)
     (~r value #:base 2 #:min-width word-size-bits #:pad-string "0")]
    [(or 'hex 'h)
     (~r value #:base 16 #:min-width word-size-bytes #:pad-string "0")]))

;; A Program is a list of instructions.
;;
;; TODO: It would be neat to define, say, [#lang a86] where the program can just
;; be directly written, and then the underlying implementation of the #lang
;; converts it into a [Program]. Don't know how practical or useful that is.
(struct Program (instructions) #:transparent)

;; The State is a recording of the input program, the registers, and memory. It
;; should be considered immutable and thus should be modified functionally.
;;
;; TODO: Is a functional data structure the move here? I'm not sure what the
;; complexity of late-semester programs is, but I think going functional may
;; make supporting advanced debugging easier?
(struct State (program registers flags memory) #:transparent)

;; The registers are represented as a hashmap from register names to values.
;; Note that the special register reference [eax] is not included; this must be
;; translated by the machine. Registers are initialized to [0].
(define register-names
  '(rax rbx rcx rdx rbp rsp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))
(define new-registers
  (apply hash (flatten (map (λ (r) (list r 0)) register-names))))

;; The flags are represented as a hashmap from flag names to values. Flags
;; should only be set to either [#t] or [#f]. Flags are initialized to [#f].
(define flag-names
  '(OF SF ZF CF))
(define new-flags
  (apply hash (flatten (map (λ (f) (list f #f)) flag-names))))

;; Memory is a stack.
;;
;; TODO: Should the memory have a bound?
(define new-memory '())

;; Given a program, returns a state.
(define (initialize-state program)
  ;; TODO: Should the program instead be converted into initial memory at this
  ;; point? I think probably yes. It seems important to actually have the
  ;; instructions in memory, since the addresses of labels can be accessed
  ;; directly via [lea]. (My initial thought was to cheat with the labels, but
  ;; the more I've considered it the less viable that seems.)
  (State program new-registers new-flags new-memory))

;; TODO: Implement this, then an [interp] function that calls it repeatedly.
(define (step state)
  state)
