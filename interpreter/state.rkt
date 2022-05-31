#lang racket

(require "register.rkt"
         racket/format)

(provide (all-defined-out))


;; The State is a recording of the labels, the registers, the flags, and memory.
;; It should be considered immutable and thus should be modified functionally.
;;
;; Fields:
;;   labels    - association list of labels to addresses
;;   registers - hashmap of register names to values
;;   flags     - hashmap of flag names to values
;;   memory    - ???
;;
;; TODO: Is a functional data structure the move here? I'm not sure what the
;; complexity of late-semester programs is, but I think going functional may
;; make supporting advanced debugging easier?
(struct State (labels registers flags memory) #:transparent)

;; Memory is a stack.
;;
;; TODO: Should the memory have an explicit bound?
;; TODO: Should the memory be pre-allocated? (Meaning a given quantity of memory
;; is initialized to contain 0s instead of being an empty list.)
(define new-memory '())

;; Given a program, returns a state.
(define (initialize-state program)
  ;; TODO: Should the program instead be converted into initial memory at this
  ;; point? I think probably yes. It seems important to actually have the
  ;; instructions in memory, since the addresses of labels can be accessed
  ;; directly via [lea]. (My initial thought was to cheat with the labels, but
  ;; the more I've considered it the less viable that seems.)
  (State '() new-registers new-flags new-memory))

;; TODO: Implement this, then an [interp] function that calls it repeatedly.
(define (step state)
  state)
