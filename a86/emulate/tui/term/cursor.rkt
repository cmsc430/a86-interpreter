#lang racket

(require "posn.rkt"
         charterm)

(define current-posn (make-parameter unit-o (λ (x) (and (posn? x) x)) 'current-posn))
