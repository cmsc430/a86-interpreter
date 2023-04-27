#lang racket

(provide convert
         ptr-ref)

(require "emulate.rkt"
         "etypes.rkt"
         "../emulator.rkt")

(define (convert v t)
  (let* ([to-width (etype-bit-width t)]
         [to-signed? (etype-signed? t)]
         [mask (sub1 (arithmetic-shift 1 to-width))]
         [masked-v (bitwise-and v mask)])
    (or (and to-signed?
             (not (zero? (arithmetic-shift masked-v (- (sub1 to-width)))))
             (- (add1 (bitwise-xor masked-v mask))))
        masked-v)))

(define (ptr-ref addr type [offset 0])
  (let ([adjusted-address (+ addr (* offset (etype-bit-width type)))])
    (convert (emulator-memory-ref (current-emulator) adjusted-address)
             type)))
