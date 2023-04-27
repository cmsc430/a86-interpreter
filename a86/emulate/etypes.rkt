#lang racket

(provide etype? etype-bit-width etype-signed?
         int32 int64 uint32 uint64)

(struct etype (bit-width signed?))
(define  int32 (etype 32 #t))
(define  int64 (etype 64 #t))
(define uint32 (etype 32 #f))
(define uint64 (etype 64 #f))
