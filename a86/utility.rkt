#lang racket

(provide word-size-bytes
         word-size-bits
         format-word
         bit-set?
         max-signed
         min-signed
         max-unsigned
         min-unsigned
         max-unsigned/32-bit
         a86-value?
         a86-value/32-bit?
         64-bit-integer?
         random-64-bit-integer
         32-bit-integer?
         address?
         sign-mask
         truncate-integer/signed
         truncate-integer/unsigned
         a86-value->signed-integer
         signed-in-bounds?
         unsigned-in-bounds?
         make-mask
         align-address-to-word
         word-aligned-offset
         greater-word-aligned-address
         lesser-word-aligned-address
         aligned-to-word?
         seq)

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
  (if (exact-integer? value)
      (match mode
        [(or 'binary 'bin 'b)
         (~r value #:base 2 #:min-width word-size-bits #:pad-string "0")]
        [(or 'hexadecimal 'hex 'h)
         (~r value #:base 16 #:min-width (* 2 word-size-bytes) #:pad-string "0")])
      (~a value)))

;; Returns a Boolean indicating whether the specific bit is set in the given
;; word.
;;
;; NOTE: This is a zero-based index, i.e., the least-significant bit is bit 0.
(define (bit-set? word bit-index)
  (not (zero? (bitwise-and word (arithmetic-shift 1 bit-index)))))

;; Maximum and minimum values for signed and unsigned representations.
;; max-signed:   011...
;; min-signed:   100...
;; max-unsigned: 111...
;; min-unsigned: 000...
(define max-signed   (sub1 (arithmetic-shift 1 (sub1 word-size-bits))))
(define min-signed         (arithmetic-shift 1 (sub1 word-size-bits)))
(define max-unsigned (sub1 (arithmetic-shift 1       word-size-bits)))
(define min-unsigned 0)
(define max-unsigned/32-bit (sub1 (arithmetic-shift 1 32)))

;; Values are integers in the 64-bit unsigned range
(define (a86-value? x)
  (<= min-unsigned x max-unsigned))

(define (a86-value/32-bit? x)
  (<= min-unsigned x max-unsigned/32-bit))

;; Whether a value is a 64-bit integer.
(define (64-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-length x) 64)))

;; A random 64-bit value.
(define (random-64-bit-integer)
  (random min-unsigned (add1 max-unsigned)))

;; Whether a value is a 32-bit integer.
(define (32-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-length x) 32)))

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
  (bitwise-ior (if (negative-integer? n) sign-mask 0)
               (bitwise-and n max-signed)))

;; Converts a signed integer representation to its unsigned representation.
;; Truncates a Racket integer for use in the machine as an unsigned integer.
(define (truncate-integer/unsigned n)
  (bitwise-and n max-unsigned))

;; Converts an unsigned integer into a signed integer in Racket.
(define (a86-value->signed-integer n)
  (if (zero? (bitwise-and n sign-mask))
      n
      (- (add1 (bitwise-xor max-unsigned n)))))

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
;;
;; The [width] determines the total bit-width of the mask. By default, masks are
;; made for words.
;;
;; The [offset] specifies an amount of bits to offset the mask by. The [offset]
;; is assumed to be non-negative. It counts from the right for non-negative
;; values of [n] and from the left for negative values of [n].
(define (make-mask n [width word-size-bits] [offset 0])
  (if (>= n 0)
      ;; Mask from the least-significant bits.
      (bitwise-xor (sub1 (arithmetic-shift 1 (+ n offset)))
                   (sub1 (arithmetic-shift 1 offset)))
      ;; Mask from the most-significant bits.
      (if (> (+ (abs n) offset) width)
          (error 'make-mask "size + offset must be less than mask width")
          (bitwise-xor (sub1 (arithmetic-shift 1 (- width offset)))
                       (sub1 (arithmetic-shift 1 (- width (abs n) offset)))))))

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
(define (seq . xs)
  (foldr (λ (x xs)
           (if (list? x)
               (append x xs)
               (cons x xs)))
         '()
         xs))
