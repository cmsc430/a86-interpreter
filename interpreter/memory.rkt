#lang racket

(provide word-size-bits
         word-size-bytes
         format-word
         next-word-aligned-address
         align-address-to-word
         aligned-to-word?
         initialize-memory
         memory-ref
         memory-set!
         unsafe-memory-set!)

;; The size of words, given in bits.
(define word-size-bits 64)
;; The size of words, given in bytes.
(define word-size-bytes (/ word-size-bits 8))

(define (format-word value [mode 'binary])
  (match mode
    [(or 'binary 'bin 'b)
     (~r value #:base 2 #:min-width word-size-bits #:pad-string "0")]
    [(or 'hexadecimal 'hex 'h)
     (~r value #:base 16 #:min-width word-size-bytes #:pad-string "0")]))

(define (next-word-aligned-address address)
  (- (align-address-to-word address) word-size-bytes))

(define (align-address-to-word address)
  (- address (modulo address word-size-bytes)))

(define (aligned-to-word? address)
  (= 0 (modulo address word-size-bytes)))

;; Memory is a mapping from Addresses to vectors of Cells. Each Cell is a pair
;; of a time tick with a value.
(struct Cell (tick value) #:transparent)

;; TODO: comment
;;
;;   max-address: the highest address available
;;   min-address: the lowest address available
;;   memhash: a box containing the hashmap from addresses to immutable vectors
;;            of Cells
;;   handling: one of the memory handling strategies
;;   max-depth: the maximum depth if a limited strategy is used
(struct Memory (max-address
                min-address
                memhash
                handling
                max-depth
                error-on-overwrite)
  #:transparent)

;; The supported memory handling strategies.
(define handling-strategy-unlimited 'unlimited)
(define handling-strategy-limited 'limited)
(define handling-strategy-rotating 'rotating)

;; Defines the maximum amount of values a Cell will hold.
(define max-cell-depth 10)

(define (initialize-memory [instructions (list)]
                           [max-address #xffffffffffffffff]
                           [min-address #xffff800000000000]
                           [handling-strategy handling-strategy-unlimited]
                           [max-depth max-cell-depth]
                           [error-on-initialized-overwrite #t])
  (let ([address-instruction-pairs
         (for/fold ([address (align-address-to-word max-address)]
                    [address-instruction-pairs (list)]
                    #:result address-instruction-pairs)
                   ([instruction instructions])
           (values (next-word-aligned-address address)
                   (cons (cons address (vector-immutable (Cell 0 instruction)))
                         address-instruction-pairs)))])
    (Memory max-address
            min-address
            (make-hash address-instruction-pairs)
            handling-strategy
            max-depth
            error-on-initialized-overwrite)))

;; An empty set of bytes.
(define (make-empty-bytes) (bytes->immutable-bytes (make-bytes word-size-bytes 0)))

;; Retrieves the value stored at the indicated address. If no value is present
;; in the hash table, an empty set of bytes are returned representing what would
;; be expected if our memory was not being emulated.
(define (memory-ref memory address [failure-result make-empty-bytes])
  (let ([result (hash-ref (Memory-memhash memory)
                          address
                          #f)])
    (if result
        (Cell-value (vector-ref result 0))
        (failure-result))))

;; Returns the number of times the indicated address has been written to.
(define (memory-depth memory address)
  (vector-length (hash-ref (Memory-memhash memory)
                           address
                           (vector-immutable))))

;; Determines whether the indicated address has been written to the maximum
;; number of times.
(define (at-max-depth? memory address)
  (let ([handling (Memory-handling memory)])
    (and (not (eq? handling handling-strategy-unlimited))
         (or (eq? handling handling-strategy-limited)
             (eq? handling handling-strategy-rotating))
         (>= (memory-depth memory address)
             (Memory-max-depth memory)))))

(define (specialized-initial-value? memory address)
  (not (bytes? (memory-ref memory address))))

;; Stores the given value (with its corresponding tick) at the indicated address
;; in memory. This is a stateful action; no value is returned.
(define (memory-set! memory address tick value)
  (cond
    [(not (Memory? memory))
     (raise-user-error 'memory-set "expected initialized memory; got ~v" memory)]
    [(not (bytes? value))
     (raise-user-error 'memory-set! "values to be stored in memory must be bytes")]
    [(> address (Memory-max-address memory))
     (raise-user-error 'memory-set! "expected address less than ~a; got ~a" (Memory-max-address memory) address)]
    [(< address (Memory-min-address memory))
     (raise-user-error 'memory-set! "expected address greater than ~a; got ~a" (Memory-min-address memory) address)]
    [(and (specialized-initial-value? memory address)
          (Memory-error-on-overwrite memory))
     (raise-user-error 'memory-set!
                       "cannot overwrite memory initialized with special value ~v at address ~a"
                       (memory-ref memory address)
                       address)]
    [(at-max-depth? memory address)
     (raise-user-error 'memory-set!
                       "address ~a has been written to the maximum allowed number of times (~a)"
                       address
                       (Memory-max-depth memory))]
    [(< tick 0)
     (raise-user-error 'memory-set!
                       "cannot use time ticks with values less than 0; got ~a" tick)]
    [else
     (hash-set! (Memory-memhash memory)
                address
                (apply vector-immutable
                       (cons (Cell tick value)
                             (vector->list (hash-ref (Memory-memhash memory)
                                                     address
                                                     (vector-immutable))))))]))

;; The same as [memory-set!], but does not perform any safety checks.
(define (unsafe-memory-set! memory address tick value)
  (hash-set! (Memory-memhash memory)
             address
             (apply vector-immutable
                    (cons (Cell tick value)
                          (vector->list (hash-ref (Memory-memhash memory)
                                                  address
                                                  (vector-immutable)))))))
