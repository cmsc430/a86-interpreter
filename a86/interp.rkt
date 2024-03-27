#lang racket
;; FIXME: This is all bad.

(provide current-objs
         asm-interp
         asm-interp/io)

(require "emulate.rkt"
         "emulate/runtime.rkt")

(current-runtime fraud)

(define current-objs void)

(define (asm-interp instructions)
  (asm-emulate instructions))

(define (asm-interp/io instructions in)
  (asm-emulate/io instructions (open-input-string in)))
