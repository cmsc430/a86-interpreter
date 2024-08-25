#lang racket

(provide word-size-bytes
         word-size-bits
         format-word
         bit-set?
         max-signed
         min-signed
         max-unsigned
         min-unsigned
         max-signed/32-bit
         min-signed/32-bit
         max-unsigned/32-bit
         min-unsigned/32-bit
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
         seq
         split-first
         filter*)

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
(define max-signed/32-bit   (sub1 (arithmetic-shift 1 31)))
(define min-signed/32-bit         (arithmetic-shift 1 31))
(define max-unsigned/32-bit (sub1 (arithmetic-shift 1 32)))
(define min-unsigned/32-bit 0)

;; Values are integers in the 64-bit unsigned range
(define (a86-value? x)
  (and (real? x)
       (<= min-unsigned x max-unsigned)))

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

;; Constructs a bit-mask of [n] bits. If [n] is non-negative, the mask is
;; constructed starting from the least-significant bit. If [n] is negative, the
;; mask is constructed starting from the most-significant bit.
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

;; Like [(split-at lst 1)], except the first result is returned as just the
;; element instead of a list.
(define (split-first lst)
  (match lst
    ['() (raise-argument-error 'split-first "a list with at least 1 element" lst)]
    [(cons x xs) (values x xs)]))

;; Filter a number of lists, such that the lists will be filtered down in step
;; with one another. For example:
;;
;;   > (filter* (list (λ (n) (> n 3))
;;                    (λ (s) (not (string-equal? s ""))))
;;       '(7 2 4 5 1)
;;       '("foo" "bar" "" "baz" "qux"))
;;   '(7 5)
;;   '("foo" "baz")
;;
;; The first argument can be one of:
;;
;;   - [#f], in which case layers will be discarded when element(s) are [#f].
;;   - A procedure, in which case layers will be discarded when the procedure
;;     returns [#f] when applied to any element in that layer.
;;   - A list of procedures of equal length to the number of lists passed in, in
;;     which case each predicate is applied only to elements of the
;;     corresponding list and layers are discarded when any predicate returns
;;     [#f].
(define (filter* preds . lsts)
  ;; Stores the next values while iterating over each layer of the [xss].
  (define nexts (make-vector (length lsts) #f))
  ;; Stores the new (pruned) result lists.
  (define rss   (make-vector (length lsts) '()))

  (define (raise-incompatible-list-length-error lst-idx)
    (raise-arguments-error 'filter* "all lists must have same size"
                           "first list length" (length (first lsts))
                           "other list length" (length (list-ref lsts lst-idx))))

  ;; Processes each [xs] in [xss] to drop layers with elements that do not pass
  ;; their corresponding predicates. The end result is the same number of lists
  ;; that were passed in, but their lengths will all be the same.
  (define (descend xss accumulated?)
    ;; If nexts were accumulated, add them into the result lists.
    (when accumulated?
      (for ([(next idx) (in-indexed (in-vector nexts))])
        (vector-set! rss idx (cons next (vector-ref rss idx)))))
    ;; Process the remainders of the lists.
    (match xss
      ;; If the list of lists is empty, we're done. Reverse the sub-lists and
      ;; return them.
      ['()
       (vector-map! reverse rss)
       (vector->values rss)]
      ;; If the first sub-list is empty, ensure all the others are also empty.
      [(cons '() xss)
       (let null-loop ([xss xss]
                       [idx 1])
         (match xss
           ;; We've completed the check successfully. Recurse one last time to
           ;; trigger the regular completion process.
           ['()            (descend xss #f)]
           ;; The next sub-list is empty, so check the remainders.
           [(cons '() xss) (null-loop xss (add1 idx))]
           ;; The next sub-list is not empty, so raise an error.
           [_              (raise-incompatible-list-length-error idx)]))]
      ;; Otherwise, process the next layer of the sub-lists.
      ;;
      ;; NOTE: We are promising that the first list is non-empty.
      [_ (accumulate xss '() 0 preds #f)]))

  ;; Iterates over each [xs] in [xss] to find all [x] that are accepted by their
  ;; corresponding [pred].
  ;;
  ;;   xss         - the list of lists to process
  ;;   yss         - a new list of lists built from the [cdr]s of the [xss], to
  ;;                 be used during the recursive call
  ;;   lst-idx     - the index of the current list in [xss] we're on
  ;;   preds       - either [#f], a predicate,  or a list of predicates
  ;;   found-next? - whether a next value has been found in this iteration
  (define (accumulate xss yss lst-idx preds found-next?)
    (match xss
      ;; If the list of lists is empty, we've completed iterating over this
      ;; layer of the sub-lists.
      ['() (descend (reverse yss) found-next?)]
      ;; If the next list is empty, we know we have an error because we were
      ;; promised that at least the first list is non-empty by [descend].
      [(cons '() _) (raise-incompatible-list-length-error lst-idx)]
      ;; Otherwise, we have at least one element in the next list. We strip it
      ;; off and check it matches the relevant predicate.
      [(cons (cons x xs) xss)
       (let-values ([(pred preds)
                     (cond
                       [(procedure? preds) (values preds preds)]
                       [(cons? preds)      (split-first preds)]
                       [(eq? preds #f)     (values identity #f)]
                       [else (raise-argument-error 'filter* "#f, a procedure, or a list of procedures" preds)])])
         (if (pred x)
             ;; The value is accepted; accumulate it.
             (begin
               (vector-set! nexts lst-idx x)
               (accumulate xss (cons xs yss) (add1 lst-idx) preds #t))
             ;; The value is not accepted; we skip this layer.
             (discard xss (cons xs yss) (add1 lst-idx))))]))

  ;; Iterates over each [xs] in [xss] to ensure every list is non-empty, but do
  ;; not retain the elements encountered. If a list is empty, raise an error.
  (define (discard xss yss lst-idx)
    (match xss
      ;; If the list of lists is empty, we've successfully discarded this layer.
      ['() (descend (reverse yss) #f)]
      ;; If the next list is empty, we have an error.
      [(cons '() _) (raise-incompatible-list-length-error lst-idx)]
      ;; Otherwise, we discard the next [x] and continue discarding.
      [(cons (cons _ xs) xss)
       (discard xss (cons xs yss) (add1 lst-idx))]))

  (descend lsts #f))

;; An extended version of [racket/syntax].
(module* racket/syntax #f
  (provide format-ids
           with-syntax-values
           with-pruned-syntax-lists
           define/syntax-parse/filtered
           (all-from-out racket/syntax))

  (require racket/syntax
           (for-syntax racket/string
                       racket/syntax
                       syntax/parse))

  ;; Like [format-id], but operates on a list of syntax objects to apply each of
  ;; them to the format string.
  (define-syntax (format-ids stx)
    (syntax-parse stx
      [(_ lctxs fmt-str (~seq kw:keyword arg) ...)
       #'(format-ids this-stx lctxs fmt-str this-stx (~@ kw arg) ...)]
      [(_ this-lctx:id lctxs fmt-str arg ...)
       #'(map (λ (lctx)
                (let ([this-lctx lctx])
                  (format-id lctx fmt-str arg ...)))
              lctxs)]))

  #;(with-syntax-values ([((x ...) (y ...))
                          (values #'(1 2 3)
                                  #'(a b c))])
      #'(foo ([x y] ...) bar))
  ;; ==>
  ;; #<syntax:string:4:4 (foo ((1 a) (2 b) (3 c)) bar)>

  (define-syntax (with-syntax-values stx)
    (syntax-parse stx
      [(_ () body ...+)
       #'(with-syntax () body ...)]
      [(_ ([{pattern ...} stx-vals-expr])
          body ...+)
       #:with (stx-vals-subexpr-var ...) (map (λ (pat-stx) (format-id pat-stx "stx-vals-subexpr-var-~a" (gensym)))
                                              (syntax->list #'(pattern ...)))
       #'(call-with-values (λ () stx-vals-expr)
                           (λ (stx-vals-subexpr-var ...)
                             (with-syntax ([pattern stx-vals-subexpr-var] ...)
                               body ...)))]
      [(_ ([{pattern0 ...} stx-vals-expr0]
           [{pattern  ...} stx-vals-expr ] ...+)
          body ...+)
       #'(with-syntax-values ([{pattern0 ...} stx-vals-expr0])
           (with-syntax-values ([{pattern ...} stx-vals-expr] ...)
             body ...))]))

  #;(with-pruned-syntax-lists ([(x ...) (list #'x #'y #'z)]
                               [(y ...) (list #'1 #f  #'3)])
      #'(foo ([x y] ...) bar))
  ;; ==>
  ;; #<syntax:string:3:8 (foo ((x 1) (z 3)) bar)>

  (define-syntax (with-pruned-syntax-lists stx)
    (syntax-parse stx
      [(_ [(pattern stx-lst-exp) ...] body ...+)
       #'(with-syntax-values ([(pattern ...) (apply filter* #f (list stx-lst-exp ...))])
           body ...)]))

  (define-syntax (define/syntax-parse/filtered stx)
    (define-syntax-class id+class
      #:attributes (name class)

      (pattern name+class:id

               #:attr parts (regexp-match #rx"^([^:]+):(.+)$" (symbol->string (syntax-e #'name+class)))

               #:fail-when (and (not (attribute parts))
                                #'name+class)
               "expected identifier with syntax class annotation"

               #:with name  (format-id #'name+class (cadr  (attribute parts)))
               #:with class (format-id #'name+class (caddr (attribute parts)))))

    (define-syntax-class ellipsis
      (pattern (~datum ...)))

    (define-syntax-class id+class-list
      #:attributes (name+class name class)

      (pattern (name+class:id+class _:ellipsis)
               #:with name  #'name+class.name
               #:with class #'name+class.class))

    (syntax-parse stx
      [(_ spec:id+class-list stx-expr)

       #:with t       (format-id #'spec "t")
       #:with t+class (format-id #'t "~a:~a" #'t #'spec.class)

       #`(define/syntax-parse (spec.name+class #,(datum->syntax #'here '...))
           (filter-map (λ (type-stx) (syntax-parse type-stx [t+class #'t] [_ #f]))
                       (syntax->list stx-expr)))])))
