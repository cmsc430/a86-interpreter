#lang racket

(require "registers.rkt"

         (for-syntax syntax/parse
                     racket/syntax))

(provide word-size-bytes
         word-size-bits
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
         make-full-mask
         unsigned-in-bounds?
         a86:add
         a86:sub
         a86:and
         word-aligned-offset
         previous-word-aligned-address
         next-word-aligned-address
         align-address-to-word
         aligned-to-word?
         sequence)

;; The size of words, given in bytes.
(define word-size-bytes (make-parameter 8))
;; The size of words, given in bits.
(define (word-size-bits) (* 8 (word-size-bytes)))

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

;; Constructs a bit-mask. If [n] is non-negative, the mask is constructed
;; starting from the least-significant bit. If [n] is negative, the mask is
;; constructed starting from the most-significant bit.
(define (make-full-mask n)
  (if (>= n 0)
      ;; Mask from the least-significant bits.
      (sub1 (arithmetic-shift 1 n))
      (bitwise-xor (max-unsigned)
                   (sub1 (arithmetic-shift 1 (- (word-size-bits)
                                                (- n)))))))

;; Determines whether masking the unsigned representation of a number loses any
;; information.
(define (unsigned-in-bounds? n)
  (= n (mask n)))

;; Provides a form for defining new binary instructions, such as Add and Sub.
(define-syntax (define-binary-instruction stx)
  (syntax-parse stx
    [(_ (name arg1 arg2)
        (~seq #:base-computation base-computation)
        (~optional (~seq #:result-name result-name)
                   #:defaults ([result-name #'value]))
        (~optional (~seq #:overflow-computation overflow-computation))
        (~optional (~seq #:sign-computation sign-computation))
        (~optional (~seq #:zero-computation zero-computation))
        (~optional (~seq #:carry-computation carry-computation)))
     (with-syntax ([func-name (format-id #'name #:source #'name "a86:~a" (syntax-e #'name))]
                   [base-result (format-id #'result-name "base-~a" (syntax-e #'result-name))]
                   [masked-result (format-id #'result-name "masked-~a" (syntax-e #'result-name))]
                   [masked-result-sign (format-id #'result-name "masked-~a-sign" (syntax-e #'result-name))]
                   [arg1-sign (format-id #'arg1 "~a-sign" (syntax-e #'arg1))]
                   [arg2-sign (format-id #'arg1 "~a-sign" (syntax-e #'arg2))])
       #'(define (func-name arg1 arg2)
           (unless (unsigned-in-bounds? arg1)
             (raise-user-error 'func-name "first argument not within word size bounds: ~a" arg1))
           (unless (unsigned-in-bounds? arg2)
             (raise-user-error 'func-name "second argument not within word size bounds: ~a" arg2))
           (let* ([sign-mask (sign)]
                  [arg1-sign (bitwise-and sign-mask arg1)]
                  [arg2-sign (bitwise-and sign-mask arg2)]
                  [base-result base-computation]
                  [masked-result (mask base-result)]
                  [masked-result-sign (bitwise-and sign-mask masked-result)]
                  [set-overflow? (~? (~@ overflow-computation)
                                     (~@ #f))]
                  [set-sign? (~? (~@ sign-computation)
                                 (~@ (not (= 0 masked-result-sign))))]
                  [set-zero? (~? (~@ zero-computation)
                                 (~@ (= 0 masked-result)))]
                  [set-carry? (~? (~@ carry-computation)
                                  (~@ (= 0 (bitwise-and base-result
                                                        (arithmetic-shift 1 (word-size-bits))))))]
                  [flags (make-new-flags #:overflow set-overflow?
                                         #:sign set-sign?
                                         #:zero set-zero?
                                         #:carry set-carry?)])
             (values masked-result flags))))]))

;; Given two integers, calculates their sum. Returns the sum along with a new
;; set of flags.
(define-binary-instruction (add a1 a2)
  #:base-computation (+ a1 a2)
  #:result-name sum
  #:overflow-computation (and (= a1-sign a2-sign)
                              (not (= a1-sign masked-sum-sign))))

;; Given two integers, calculates their difference. Returns the difference along
;; with a new set of flags.
(define-binary-instruction (sub a1 a2)
  #:base-computation (- a1 a2)
  #:result-name diff
  #:overflow-computation (or (and (= 0 a1-sign)
                                  (not (= 0 a2-sign))
                                  (not (= 0 masked-diff-sign)))
                             (and (not (= 0 a1-sign))
                                  (= 0 a2-sign)
                                  (= 0 masked-diff-sign))))

;; Given two integers, calculates their bitwise product. Returns the product
;; along with a new set of flags.
(define-binary-instruction (and a1 a2)
  #:base-computation (bitwise-and a1 a2)
  #:result-name prod
  #:carry-computation #f)

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

;; Combine lists and individual elements willy-nilly.
(define (sequence . xs)
  (foldr (λ (x xs)
           (if (list? x)
               (append x xs)
               (cons x xs)))
         '()
         xs))
