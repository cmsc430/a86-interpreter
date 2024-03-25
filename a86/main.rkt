#lang racket

(require "ast.rkt"
         "registers.rkt"
         "emulate.rkt"
         "emulate/runtime.rkt"
         "printer.rkt")

;; FIXME: This is all bad.
(current-runtime fraud)

(define current-objs void)

(define (asm-interp instructions)
  (asm-emulate instructions))

(define (asm-interp/io instructions in)
  (asm-emulate/io instructions (open-input-string in)))

(provide (all-from-out "ast.rkt"
                       "registers.rkt"
                       "printer.rkt")
         ;; FIXME
         current-objs
         asm-interp
         asm-interp/io)
