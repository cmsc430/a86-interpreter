#lang racket

(require "ast.rkt"
         "registers.rkt")

(provide word-size-bits
         word-size-bytes
         format-word
         max-signed
         min-signed
         max-unsigned
         min-unsigned
         make-empty-bytes
         sign
         integer->signed
         integer->unsigned
         mask
         unsigned-in-bounds?
         bitwise-add
         bitwise-sub
         word-aligned-offset
         previous-word-aligned-address
         next-word-aligned-address
         align-address-to-word
         aligned-to-word?
         address-from-offset)

;; The size of words, given in bits.
(define word-size-bits (make-parameter 64))
;; The size of words, given in bytes.
(define (word-size-bytes) (/ (word-size-bits) 8))

;; Given either an integer or a byte string, returns a string representing that
;; value in either binary or hexadecimal.
;;
;;   mode:
;;       determines how to format the value; accepted formats are
;;           'binary OR 'bin OR 'b      - binary formatting
;;           'hexadecimal OR 'hex OR 'h - hexadecimal formatting
(define (format-word value [mode 'binary])
  (cond
    [(bytes? value)
     (unless (= (bytes-length value) (word-size-bytes))
       (raise-user-error 'format-word
                         "given byte string contains ~a bytes; expected ~a"
                         (bytes-length value)
                         (word-size-bytes)))
     (apply string-append
            (map (λ (b) (format-word b mode 1))
                 (bytes->list value)))]
    [(integer? value)
     (unless (>= value 0)
       (raise-user-error 'format-word
                         "given negative integer ~a"
                         value))
     (unless (<= value (sub1 (expt 2 (* 8 (word-size-bytes)))))
       (raise-user-error 'format-word
                         "given too-large integer ~a"
                         value))
     (match mode
       [(or 'binary 'bin 'b)
        (~r value #:base 2 #:min-width (word-size-bits) #:pad-string "0")]
       [(or 'hexadecimal 'hex 'h)
        (~r value #:base 16 #:min-width (word-size-bytes) #:pad-string "0")])]))


;; Maximum and minimum values for signed and unsigned representations.
(define (max-signed) (sub1 (arithmetic-shift 1 (sub1 (word-size-bits)))))
(define (min-signed) (arithmetic-shift 1 (sub1 (word-size-bits))))
(define (max-unsigned) (sub1 (arithmetic-shift 1 (word-size-bits))))
(define (min-unsigned) 0)

;; Produces an empty set of bytes.
(define (make-empty-bytes) (bytes->immutable-bytes (make-bytes (word-size-bytes) 0)))

;; A mask for the sign bit.
(define (sign) (arithmetic-shift 1 (sub1 (word-size-bits))))

;; Converts an unsigned integer representation to its signed representation.
(define (integer->signed n)
  (let ([sign (sign)])
    (- (bitwise-and n (sub1 sign))
       (bitwise-and n sign))))

;; Converts a signed integer representation to its unsigned representation.
(define (integer->unsigned n)
  (let ([sign (sign)]
        [signed (integer->signed n)])
    (bitwise-ior (bitwise-and n (sub1 sign))
                 (bitwise-and signed sign))))

;; Masks a number such that its value is truncated to only the bits supported
;; with the current word size.
(define (mask n)
  (bitwise-and (integer->unsigned n)
               (sub1 (arithmetic-shift 1 (word-size-bits)))))

;; Determines whether masking the unsigned representation of a number loses any
;; information.
(define (unsigned-in-bounds? n)
  (= n (mask n)))

;; Given two integers, calculates their sum. Returns the sum along with a new
;; set of flags.
(define (bitwise-add a1 a2)
  (cond
    [(not (unsigned-in-bounds? a1))
     (raise-user-error 'bitwise-add "first argument not within word size bounds: ~a" a1)]
    [(not (unsigned-in-bounds? a2))
     (raise-user-error 'bitwise-add "second argument not within word size bounds: ~a" a2)]
    [else
     (let* ([sign (sign)]
            [base-sum (+ a1 a2)]
            [masked-sum (mask base-sum)]
            [a1-sign (bitwise-and sign a1)]
            [a2-sign (bitwise-and sign a2)]
            [args-same-sign (= a1-sign a2-sign)]
            [sum-sign (bitwise-and sign masked-sum)]
            [set-overflow (and args-same-sign
                               (not (= a1-sign sum-sign)))]
            [set-sign (not (= 0 sum-sign))]
            [set-zero (= 0 masked-sum)]
            [set-carry (not (= 0 (bitwise-and base-sum
                                              (arithmetic-shift 1 (word-size-bits)))))]
            [flags (make-new-flags #:overflow set-overflow
                                   #:sign set-sign
                                   #:zero set-zero
                                   #:carry set-carry)])
       (values masked-sum flags))]))

;; Given two integers, calculates their difference. Returns the difference along
;; with a new set of flags.
(define (bitwise-sub a1 a2)
  (cond
    [(not (unsigned-in-bounds? a1))
     (raise-user-error 'bitwise-sub "first argument not within word size bounds: ~a" a1)]
    [(not (unsigned-in-bounds? a2))
     (raise-user-error 'bitwise-sub "second argument not within word size bounds: ~a" a2)]
    [else
     (let* ([sign (sign)]
            [base-diff (- a1 a2)]
            [masked-diff (mask base-diff)]
            [a1-sign (bitwise-and sign a1)]
            [a2-sign (bitwise-and sign a2)]
            [diff-sign (bitwise-and sign masked-diff)]
            [set-overflow (or (and (= 0 a1-sign)
                                   (not (= 0 a2-sign))
                                   (not (= 0 diff-sign)))
                              (and (not (= 0 a1-sign))
                                   (= 0 a2-sign)
                                   (= 0 diff-sign)))]
            [set-sign (not (= 0 diff-sign))]
            [set-zero (= 0 masked-diff)]
            [set-carry (not (= 0 (bitwise-and base-diff
                                              (arithmetic-shift 1 (word-size-bits)))))]
            [flags (make-new-flags #:overflow set-overflow
                                   #:sign set-sign
                                   #:zero set-zero
                                   #:carry set-carry)])
       (values masked-diff flags))]))

;; Given an address, produces the word-aligned address that is [offset-in-words]
;; words lower than the given' address word-aligned address.
(define (word-aligned-offset address offset-in-words)
  (- (align-address-to-word address) (* (word-size-bytes) offset-in-words)))

;; Given an address, produces the previous word-aligned address from the given
;; address's word-aligned address.
(define (previous-word-aligned-address address)
  (word-aligned-offset address -1))

;; Given an address, produces the next word-aligned address from the given
;; address's word-aligned address.
(define (next-word-aligned-address address)
  (word-aligned-offset address 1))

;; Given an address, produces the next lowest word-aligned address, according to
;; the value of [word-size-bytes]. If the given address is word-aligned, it is
;; returned unchanged.
(define (align-address-to-word address)
  (- address (modulo address (word-size-bytes))))

;; Determines whether an address is properly word-aligned, according to the
;; value of [word-size-bytes].
(define (aligned-to-word? address)
  (= 0 (modulo address (word-size-bytes))))

;; Calculates an offset address.
(define (address-from-offset registers offset)
  (+ (Offset-i offset) (hash-ref registers (Offset-r offset))))
