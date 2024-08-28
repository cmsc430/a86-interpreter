#lang racket

(require "../utility.rkt"

         "memory.rkt"
         "runtime.rkt"
         "type-tools.rkt")

(provide (all-from-out "runtime.rkt")

         (contract-out
          [abscond   runtime?]
          [blackmail runtime?]
          [con       runtime?]
          [dupe      runtime?]
          [dodger    runtime?]
          [evildoer  runtime?]
          [extort    runtime?]
          [fraud     runtime?]
          [hoax      runtime?]
          [hustle    runtime?]
          [iniquity  runtime?]
          [jig       runtime?]
          [knock     runtime?]
          [loot      runtime?]
          ;; [hoodwink  runtime?]
          [mug       runtime?]))


(define-runtimes (abscond blackmail con))

(define-runtimes (dupe)
  #:values ([#t #b01]
            [#f #b11])
  #:types ([#:name int
            #:shift 1
            #:tag #b0
            #:predicate integer?
            #:encode (λ (v)
                       (arithmetic-shift v int-shift))
            #:decode (λ (b)
                       (arithmetic-shift b (- int-shift)))]))

(define-runtimes (dodger)
  #:values ([#t #b011]
            [#f #b111])
  #:types ([#:name int
            #:shift 1
            #:tag #b0
            #:predicate integer?
            #:encode (λ (v)
                       (arithmetic-shift v int-shift))
            #:decode (λ (b)
                       (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift 2
            #:tag #b01
            #:predicate char?
            #:encode (λ (v)
                       (bitwise-ior char-tag
                                    (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b)
                       (integer->char (arithmetic-shift b (- char-shift))))]))

(define-runtimes (evildoer)
  #:values ([#t      #b011]
            [#f      #b111]
            [eof    #b1011]
            [(void) #b1111])
  #:types ([#:name int
            #:shift 1
            #:tag #b0
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift 2
            #:tag #b01
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))])
  #:functions ([#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]))

(define-runtimes (extort fraud)
  #:values ([#t      #b011]
            [#f      #b111]
            [eof    #b1011]
            [(void) #b1111])
  #:types ([#:name int
            #:shift 1
            #:tag #b0
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift 2
            #:tag #b01
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))])
  #:functions ([#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]
               [(raise_error) (raise 'err)]))

(define-runtimes (hustle)
  #:values ([#t     #b00011000]
            [#f     #b00111000]
            [eof    #b01011000]
            [(void) #b01111000]
            ['()    #b10011000])
  #:types ([#:name ptr #:shift 3]
           [#:name imm #:shift ptr-shift]
           [#:name int
            #:shift (+ 1 imm-shift)
            #:tag #b0000
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift (+ 2 imm-shift)
            #:tag #b01000
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))]
           [#:name box
            #:shift ptr-shift
            #:tag #b001
            #:predicate box?
            #:decode (λ (b) (box (bits->value (heap-ref b))))
            #:description (λ (b) 'box)]
           [#:name cons
            #:shift ptr-shift
            #:tag #b010
            #:predicate cons?
            #:decode (λ (b) (cons (bits->value (heap-ref (+ b 8)))
                                  (bits->value (heap-ref b))))
            #:description (λ (b) 'cons)])
  #:functions ([(untag i)
                (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                                  (integer-length ptr-mask))]
               [(heap-ref a)
                (ptr-ref (untag a) int64)]
               [(char-ref a o)
                (integer->char (ptr-ref (untag a) uint32 o))]
               [#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]
               [(raise_error) (raise 'err)]))

(define-runtimes (hoax iniquity jig knock)
  #:values ([#t     #b00011000]
            [#f     #b00111000]
            [eof    #b01011000]
            [(void) #b01111000]
            ['()    #b10011000])
  #:types ([#:name ptr #:shift 3]
           [#:name imm #:shift ptr-shift]
           [#:name int
            #:shift (+ 1 imm-shift)
            #:tag #b0000
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift (+ 2 imm-shift)
            #:tag #b01000
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))]
           [#:name box
            #:shift ptr-shift
            #:tag #b001
            #:predicate box?
            #:decode (λ (b) (box (bits->value (heap-ref b))))
            #:description (λ (b) 'box)]
           [#:name cons
            #:shift ptr-shift
            #:tag #b010
            #:predicate cons?
            #:decode (λ (b) (cons (bits->value (heap-ref (+ b 8)))
                                  (bits->value (heap-ref b))))
            #:description (λ (b) 'cons)]
           [#:name vect
            #:shift ptr-shift
            #:tag #b011
            #:predicate vector?
            #:decode (λ (b) (if (zero? (untag b))
                                (vector)
                                (build-vector (heap-ref b)
                                              (λ (j)
                                                (bits->value (heap-ref (+ b (* 8 (add1 j)))))))))]
           [#:name str
            #:shift ptr-shift
            #:tag #b100
            #:predicate string?
            #:decode (λ (b) (if (zero? (untag b))
                                (string)
                                (build-string (heap-ref b)
                                              (λ (j)
                                                (char-ref (+ b 8) j)))))])
  #:functions ([(untag i)
                (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                                  (integer-length ptr-mask))]
               [(heap-ref a)
                (ptr-ref (untag a) int64)]
               [(char-ref a o)
                (integer->char (ptr-ref (untag a) uint32 o))]
               [#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]
               [(raise_error) (raise 'err)]))

(define-runtimes (loot)
  #:values ([#t     #b00011000]
            [#f     #b00111000]
            [eof    #b01011000]
            [(void) #b01111000]
            ['()    #b10011000])
  #:types ([#:name ptr #:shift 3]
           [#:name imm #:shift ptr-shift]
           [#:name int
            #:shift (+ 1 imm-shift)
            #:tag #b0000
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift (+ 2 imm-shift)
            #:tag #b01000
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))]
           [#:name box
            #:shift ptr-shift
            #:tag #b001
            #:predicate box?
            #:decode (λ (b) (box (bits->value (heap-ref b))))
            #:description (λ (b) 'box)]
           [#:name cons
            #:shift ptr-shift
            #:tag #b010
            #:predicate cons?
            #:decode (λ (b) (cons (bits->value (heap-ref (+ b 8)))
                                  (bits->value (heap-ref b))))
            #:description (λ (b) 'cons)]
           [#:name vect
            #:shift ptr-shift
            #:tag #b011
            #:predicate vector?
            #:decode (λ (b) (if (zero? (untag b))
                                (vector)
                                (build-vector (heap-ref b)
                                              (λ (j)
                                                (bits->value (heap-ref (+ b (* 8 (add1 j)))))))))
            #:description (λ (b) 'vect)]
           [#:name str
            #:shift ptr-shift
            #:tag #b100
            #:predicate string?
            #:decode (λ (b) (if (zero? (untag b))
                                (string)
                                (build-string (heap-ref b)
                                              (λ (j)
                                                (char-ref (+ b 8) j)))))
            #:description (λ (b) 'str)]
           [#:name proc
            #:shift ptr-shift
            #:tag #b101
            #:predicate procedure?
            #:description (λ (b) 'proc)])
  #:functions ([(untag i)
                (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                                  (integer-length ptr-mask))]
               [(heap-ref a)
                (ptr-ref (untag a) int64)]
               [(char-ref a o)
                (integer->char (ptr-ref (untag a) uint32 o))]
               [#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]
               [(raise_error) (raise 'err)]))

(define-runtimes (mug)
  #:values ([#t     #b00011000]
            [#f     #b00111000]
            [eof    #b01011000]
            [(void) #b01111000]
            ['()    #b10011000])
  #:types ([#:name ptr #:shift 3]
           [#:name imm #:shift ptr-shift]
           [#:name int
            #:shift (+ 1 imm-shift)
            #:tag #b0000
            #:predicate integer?
            #:encode (λ (v) (arithmetic-shift v int-shift))
            #:decode (λ (b) (arithmetic-shift b (- int-shift)))]
           [#:name char
            #:shift (+ 2 imm-shift)
            #:tag #b01000
            #:predicate char?
            #:encode (λ (v) (bitwise-ior char-tag (arithmetic-shift (char->integer v) char-shift)))
            #:decode (λ (b) (integer->char (arithmetic-shift b (- char-shift))))]
           [#:name box
            #:shift ptr-shift
            #:tag #b001
            #:predicate box?
            #:decode (λ (b) (box (bits->value (heap-ref b))))
            #:description (λ (b) 'box)]
           [#:name cons
            #:shift ptr-shift
            #:tag #b010
            #:predicate cons?
            #:decode (λ (b) (cons (bits->value (heap-ref (+ b 8)))
                                  (bits->value (heap-ref b))))
            #:description (λ (b) 'cons)]
           [#:name vect
            #:shift ptr-shift
            #:tag #b011
            #:predicate vector?
            #:decode (λ (b) (if (zero? (untag b))
                                (vector)
                                (build-vector (heap-ref b)
                                              (λ (j)
                                                (bits->value (heap-ref (+ b (* 8 (add1 j)))))))))
            #:description (λ (b) 'vect)]
           [#:name str
            #:shift ptr-shift
            #:tag #b100
            #:predicate string?
            #:decode (λ (b) (if (zero? (untag b))
                                (string)
                                (build-string (heap-ref b)
                                              (λ (j)
                                                (char-ref (+ b 8) j)))))
            #:description (λ (b) 'str)]
           [#:name proc
            #:shift ptr-shift
            #:tag #b101
            #:predicate procedure?
            #:description (λ (b) 'proc)]
           [#:name symb
            #:shift ptr-shift
            #:tag #b110
            #:predicate symbol?
            #:decode (λ (b) (string->symbol
                             (if (zero? (untag b))
                                 (string)
                                 (build-string (heap-ref b)
                                               (λ (j)
                                                 (char-ref (+ b 8) j))))))
            #:description (λ (b) 'symb)])
  #:functions ([(untag i)
                (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                                  (integer-length ptr-mask))]
               [(heap-ref a)
                (ptr-ref (untag a) int64)]
               [(char-ref a o)
                (integer->char (ptr-ref (untag a) uint32 o))]
               [(calloc num size)
                (let*-values ([(base-bytes extra-bytes)
                               (quotient/remainder (* num size) word-size-bytes)]
                              [(number-of-words)
                               (+ base-bytes (if (zero? extra-bytes) 0 1))])
                  (memory-calloc! (runtime/memory) number-of-words))]
               [(intern_symbol symb-ptr)
                (let loop ([prev     #f]
                           [prev-dir #f]
                           [curr symbol-table])
                  (match curr
                    [#f
                     (let ([n (Node symb-ptr #f #f)])
                       (if prev
                           (case prev-dir
                             [(l) (set-Node-left!  prev n)]
                             [(r) (set-Node-right! prev n)])
                           (set! symbol-table n)))]
                    [(Node elem-ptr l r)
                     (match (symb_cmp symb-ptr elem-ptr)
                       [0 elem-ptr]
                       [(? negative?) (loop curr 'l l)]
                       [(? positive?) (loop curr 'r r)])]))]
               [(symb_cmp s1-ptr s2-ptr)
                (if (= s1-ptr s2-ptr)
                    ;; If the pointers point to the same point, they're equal.
                    0
                    ;; Otherwise, we have to compare the elements.
                    (let* ([len1 ((runtime/memory-ref) s1-ptr)]
                           [len2 ((runtime/memory-ref) s2-ptr)]
                           [len  (min len1 len2)])
                      (let loop ([i 1])
                        (if (<= i len)
                            ;; Check the next element.
                            (let ([c1 ((runtime/memory-ref) (word-aligned-offset s1-ptr i))]
                                  [c2 ((runtime/memory-ref) (word-aligned-offset s2-ptr i))])
                              (if (= c1 c2)
                                  ;; Elements are equal, so we continue.
                                  (loop (add1 i))
                                  ;; Return the difference.
                                  (- c1 c2)))
                            ;; We've iterated through the elements; return the
                            ;; difference in the lengths.
                            (- len1 len2)))))]
               [#:uses-input
                (read_byte) (value->bits (read-byte))]
               [#:uses-input
                (peek_byte) (value->bits (peek-byte))]
               [#:uses-output
                (write_byte b) (value->bits (write-byte (bits->value b)))]
               [(raise_error) (raise 'err)])
  ;; The [Node] and [symbol-table] are used by [intern_symbol] and [symb_cmp].
  (struct Node (elem-ptr [left #:mutable] [right #:mutable]))
  (define symbol-table #f))
