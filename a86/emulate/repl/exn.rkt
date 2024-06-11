#lang racket

(require "../../exn.rkt")

(define-a86-exn/provide user:repl ())
(define-a86-exn/provide user:repl:bad-command (name args))
