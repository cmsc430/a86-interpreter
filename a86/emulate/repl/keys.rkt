#lang racket

(require "tty.rkt")

;; FIXME
(provide (all-defined-out))

(define (read-key)
  (integer->char (tty:read-byte))
  #;(with-raw-input
    (integer->char (tty:read-byte))))

(define (read-keys-until until-key)
  (let loop ([ks '()])
    (let ([k (read-key)])
      (if (eq? k until-key)
          (reverse ks)
          (loop (cons k ks))))))

(define (read-line)
  (read-keys-until #\return))
