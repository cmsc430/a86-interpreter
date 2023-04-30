#lang racket

(provide (struct-out StepState))

;; The current state of the interpreter.
(struct StepState (time-tick ip flags registers) #:transparent)
