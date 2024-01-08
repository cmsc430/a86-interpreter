#lang racket

(provide (all-from-out "etypes.rkt")

         ptr-ref)

(require "emulator.rkt"
         "etypes.rkt")

(define (ptr-ref addr type [offset 0])
  (let ([adjusted-address (+ addr (* offset
                                     (/ (etype-bit-width type) 8)))])
    (convert (emulator-memory-ref (current-emulator) adjusted-address) type)))