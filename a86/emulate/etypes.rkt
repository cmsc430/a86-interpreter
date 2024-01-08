#lang racket

(provide etype? etype-bit-width etype-signed?
         char uchar int32 int64 uint32 uint64
         convert)

(struct etype (bit-width signed?))
(define   char (etype  8 #t))
(define  uchar (etype  8 #f))
(define  int32 (etype 32 #t))
(define  int64 (etype 64 #t))
(define uint32 (etype 32 #f))
(define uint64 (etype 64 #f))

(define (convert v t)
  (let* ([to-width (etype-bit-width t)]
         [to-signed? (etype-signed? t)]
         [mask (sub1 (arithmetic-shift 1 to-width))]
         [masked-v (bitwise-and v mask)])
    (or (and to-signed?
             (not (zero? (arithmetic-shift masked-v (- (sub1 to-width)))))
             (- (add1 (bitwise-xor masked-v mask))))
        masked-v)))
