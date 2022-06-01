#lang racket

(provide word-size-bits
         word-size-bytes
         format-word
         word-aligned-offset
         next-word-aligned-address
         align-address-to-word
         aligned-to-word?
         initialize-memory
         memory-ref
         memory-depth
         at-max-depth?
         specialized-initial-value?
         initialized?
         memory-set!
         unsafe-memory-set!
         memory-map)

;; The size of words, given in bits.
(define word-size-bits 64)
;; The size of words, given in bytes.
(define word-size-bytes (/ word-size-bits 8))

;; Given either an integer or a byte string, returns a string representing that
;; value in either binary or hexadecimal.
;;
;;   mode:
;;       determines how to format the value; accepted formats are
;;           'binary OR 'bin OR 'b      - binary formatting
;;           'hexadecimal OR 'hex OR 'h - hexadecimal formatting
;;
;;   word-size:
;;       the number of bytes that correspond to one word
(define (format-word value [mode 'binary] [word-size word-size-bytes])
  (cond
    [(bytes? value)
     (unless (= (bytes-length value) word-size)
       (raise-user-error 'format-word
                         "given byte string contains ~a bytes; expected ~a"
                         (bytes-length value)
                         word-size))
     (apply string-append
            (map (λ (b) (format-word b mode 1))
                 (bytes->list value)))]
    [(integer? value)
     (unless (> value 0)
       (raise-user-error 'format-word
                         "given negative integer ~a"
                         value))
     (unless (< value (sub1 (expt 2 (* 8 word-size))))
       (raise-user-error 'format-word
                         "given too-large integer ~a"
                         value))
     (match mode
       [(or 'binary 'bin 'b)
        (~r value #:base 2 #:min-width (* 8 word-size) #:pad-string "0")]
       [(or 'hexadecimal 'hex 'h)
        (~r value #:base 16 #:min-width word-size #:pad-string "0")])]))

;; Given an address, produces the word-aligned address that is [offset-in-words]
;; words lower than the given' address word-aligned address.
(define (word-aligned-offset address offset-in-words)
  (- (align-address-to-word address) (* word-size-bytes offset-in-words)))

;; Given an address, produces the next word-aligned address from the given
;; address's word-aligned address.
(define (next-word-aligned-address address)
  (word-aligned-offset address 1))

;; Given an address, produces the next lowest word-aligned address, according to
;; the value of [word-size-bytes]. If the given address is word-aligned, it is
;; returned unchanged.
(define (align-address-to-word address)
  (- address (modulo address word-size-bytes)))

;; Determines whether an address is properly word-aligned, according to the
;; value of [word-size-bytes].
(define (aligned-to-word? address)
  (= 0 (modulo address word-size-bytes)))

(struct address-stream (hi-addr lo-addr)
  #:guard (λ (ha la _)
            ;; TODO: Add checks to ensure the stream will behave well.
            (values (align-address-to-word ha)
                    (align-address-to-word la)))
  #:methods gen:stream
  [(define (stream-empty? stream)
     (= (align-address-to-word (address-stream-hi-addr stream))
        (align-address-to-word (address-stream-lo-addr stream))))
   (define (stream-first stream)
     (address-stream-hi-addr stream))
   (define (stream-rest stream)
     (address-stream (next-word-aligned-address (address-stream-hi-addr stream))
                     (address-stream-lo-addr stream)))])

;; A pair of a time tick with a value.
;; TODO: Remove transparency?
(struct Cell (tick value) #:transparent)

;; A representation of runtime memory.
;;
;;   max-address:
;;       The highest address available.
;;
;;   min-address:
;;       The lowest address available.
;;
;;   memhash:
;;       A mutable hashmap from addresses to immutable vectors of Cells.
;;
;;   handling:
;;       One of the memory handling strategies.
;;
;;   max-depth:
;;       The maximum depth to be used if a non-unlimited strategy is employed.
;;
;;   error-on-overwrite:
;;       Whether to prevent attempts by [memory-set!] to overwrite addresses
;;       that currently hold non-byte-string values.
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

;; Sets up the memory to be used during emulation. All arguments are optional.
;; Returns two values: the next free word-aligned address, and a [Memory] struct
;; that should be passed to all the memory-related functions.
;;
;;   instructions:
;;       A list of instructions to include in the initial (highest) memory
;;       addresses. Anything given in this list will be written without first
;;       checking if the value is a [bytes?].
;;
;;       default: (list)
;;
;;   max-address:
;;       The highest address available. This is the "first" address to be used,
;;       rounded down to the nearest word boundary.
;;
;;       default: #xffffffffffffffff
;;
;;   min-address:
;;       The lowest address available. Memory cannot be written beyond this.
;;
;;       default: #xffff800000000000
;;
;;   handling-strategy:
;;       Memory can be set up to only allow a specific number of writes to a
;;       given address in memory. This is to help with performance, since old
;;       (overwritten) values can be kept around to help with debugging. The
;;       possible options are:
;;           'unlimited - there is no bound on the number of writes
;;           'limited   - only [max-depth] writes are allowed, after which an
;;                        error will be raised
;;           'rotating  - the address's history is treated as a rotating buffer
;;                        with only [max-depth] entries, meaning once
;;                        [max-depth] writes are performed, new writes will
;;                        "push out" the oldest entries
;;
;;       default: 'unlimited
;;
;;   max-depth:
;;       The maximum amount of writes to allow, depending on the
;;       [handling-strategy] being used.
;;
;;       default: 10
(define (initialize-memory [instructions (list)]
                           [max-address #xffffffffffffffff]
                           [min-address #xffff800000000000]
                           [handling-strategy handling-strategy-unlimited]
                           [max-depth max-cell-depth]
                           [error-on-initialized-overwrite #t])
  (let-values
      ([(next-address address-instruction-pairs)
        (for/fold ([address (align-address-to-word max-address)]
                   [address-instruction-pairs (list)])
                  ([instruction instructions])
          (values (next-word-aligned-address address)
                  (cons (cons address (vector-immutable (Cell 0 instruction)))
                        address-instruction-pairs)))])
    (values next-address
            (Memory max-address
                    min-address
                    (make-hash address-instruction-pairs)
                    handling-strategy
                    max-depth
                    error-on-initialized-overwrite))))

;; Produces an empty set of bytes.
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

;; Determines whether the indicated address holds a special (non-byte-string)
;; value. Since instructions are written into memory during memory
;; initialization, this is a way to check if the address is holding an
;; instruction.
(define (specialized-initial-value? memory address)
  (not (bytes? (memory-ref memory address))))

;; Determines whether the indicated address actually holds a value.
(define (initialized? memory address)
  (or (memory-ref memory address #f)
      #t))

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
     (unsafe-memory-set! memory address tick value)]))

;; The same as [memory-set!], but does not perform any safety checks.
(define (unsafe-memory-set! memory address tick value)
  (hash-set! (Memory-memhash memory)
             address
             (apply vector-immutable
                    (cons (Cell tick value)
                          (vector->list (hash-ref (Memory-memhash memory)
                                                  address
                                                  (vector-immutable)))))))

;; Maps over the values contained in memory from [hi-address] to [lo-address].
;; If either address is left as [#f], the corresponding default from the
;; [memory] is used.
;;
;; NOTE: The [proc] procedure should be a single-argument function that takes in
;; a memory value. For most values this will be a byte string, but be careful if
;; your mapping encompasses any special initialized memory.
(define (memory-map proc memory [hi-address #f] [lo-address #f])
  (unless hi-address
    (set! hi-address (Memory-max-address memory)))
  (unless lo-address
    (set! lo-address (Memory-min-address memory)))
  (let ([stream (address-stream hi-address lo-address)])
    (stream->list (stream-map (λ (address) (proc (memory-ref memory address))) stream))))
