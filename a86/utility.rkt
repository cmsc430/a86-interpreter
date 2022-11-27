#lang racket

(provide word-size-bytes
         word-size-bits
         format-word
         max-signed
         min-signed
         max-unsigned
         min-unsigned
         a86-value?
         address?
         sign-mask
         truncate-integer/signed
         truncate-integer/unsigned
         unsigned-in-bounds?
         make-mask
         align-address-to-word
         word-aligned-offset
         greater-word-aligned-address
         lesser-word-aligned-address
         aligned-to-word?
         sequence)

;; The size of words, given in bytes.  This language is defined only for 64-bit
;; architectures, so we use 8-byte words.
(define word-size-bytes 8)
;; The size of words, given in bits.
(define word-size-bits (* 8 word-size-bytes))

;; Given an integer?, returns a string representing that value in either binary
;; or hexadecimal.
;;
;;   mode:
;;       determines how to format the value; accepted formats are
;;           'binary OR 'bin OR 'b      - binary formatting
;;           'hexadecimal OR 'hex OR 'h - hexadecimal formatting
(define (format-word value [mode 'binary])
  (match mode
    [(or 'binary 'bin 'b)
     (~r value #:base 2 #:min-width word-size-bits #:pad-string "0")]
    [(or 'hexadecimal 'hex 'h)
     (~r value #:base 16 #:min-width word-size-bytes #:pad-string "0")]))

;; Maximum and minimum values for signed and unsigned representations.
;; max-signed:   011...
;; min-signed:   100...
;; max-unsigned: 111...
1;; min-unsigned: 000...
(define max-signed   (sub1 (arithmetic-shift 1 (sub1 word-size-bits))))
(define min-signed         (arithmetic-shift 1 (sub1 word-size-bits)))
(define max-unsigned (sub1 (arithmetic-shift 1       word-size-bits)))
(define min-unsigned 0)

;; Values are integers that exist either in the signed or unsigned ranges.
(define (a86-value? x)
  (and (integer? x)
       (or (and (negative? x)
                (>= x min-signed)
                (<= x max-signed))
           (and (>= x min-unsigned)
                (<= x max-unsigned)))))

;; Addresses can be any unsigned integer values.
(define (address? x)
  (and (integer? x)
       (>= x min-unsigned)
       (<= x max-unsigned)))

;; A mask for the sign bit. (This is equivalent to the smallest signed value.)
(define sign-mask min-signed)

;; Truncates a Racket integer for use in the machine as a signed integer. The
;; operation preserves the sign of the input.
(define (truncate-integer/signed n)
  (bitwise-ior (bitwise-and n max-signed)
               (bitwise-and n sign-mask)))

;; Converts a signed integer representation to its unsigned representation.
;; Truncates a Racket integer for use in the machine as an unsigned integer.
(define (truncate-integer/unsigned n)
  (bitwise-and n max-unsigned))

;; Determines whether masking the signed representation of a number loses any
;; information.
(define (signed-in-bounds? n)
  (= n (truncate-integer/signed n)))

;; Determines whether masking the unsigned representation of a number loses any
;; information.
(define (unsigned-in-bounds? n)
  (= n (truncate-integer/unsigned n)))

;; Constructs a bit-mask. If [n] is non-negative, the mask is constructed
;; starting from the least-significant bit. If [n] is negative, the mask is
;; constructed starting from the most-significant bit.
(define (make-mask n)
  (if (>= n 0)
      ;; Mask from the least-significant bits.
      (sub1 (arithmetic-shift 1 n))
      (bitwise-xor max-unsigned
                   (sub1 (arithmetic-shift 1 (- word-size-bits
                                                (- n)))))))

;; Given an address, produces the next lowest word-aligned address, according to
;; the value of [word-size-bytes]. If the given address is word-aligned, it is
;; returned unchanged.
(define (align-address-to-word address)
  (- address (modulo address word-size-bytes)))

;; Given an address, produces the word-aligned address that is [offset-in-words]
;; words lower than the given' address word-aligned address.
(define (word-aligned-offset address offset-in-words)
  (+ (align-address-to-word address) (* word-size-bytes offset-in-words)))

;; Given an address, produces the previous word-aligned address from the given
;; address's word-aligned address.
(define (greater-word-aligned-address address)
  (word-aligned-offset address 1))

;; Given an address, produces the next word-aligned address from the given
;; address's word-aligned address.
(define (lesser-word-aligned-address address)
  (word-aligned-offset address -1))

;; Determines whether an address is properly word-aligned, according to the
;; value of [word-size-bytes].
(define (aligned-to-word? address)
  (= 0 (modulo address word-size-bytes)))

;; Combines lists and individual elements willy-nilly.
(define (sequence . xs)
  (foldr (λ (x xs)
           (if (list? x)
               (append x xs)
               (cons x xs)))
         '()
         xs))
