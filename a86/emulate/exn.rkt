#lang racket

(require "../exn.rkt")

(define-a86-exn/provide emulator ())

(define-a86-exn/provide emulator:segfault ()
  #:parent-name exn:fail:a86:emulator)

(define-a86-exn/provide emulator:resume ()
  #:parent-name exn:fail:a86:emulator)

(define-a86-exn/provide emulator:out-of-steps ()
  #:parent-name exn:fail:a86:emulator)
