#lang racket

(provide handling-strategy-limited
         handling-strategy-rotating
         handling-strategy-unlimited
         handling-strategy
         max-cell-depth
         text rodata data bss heap stack
         read-only-sections
         read-write-sections
         static-sections
         dynamic-sections
         downward-sections
         upward-sections
         make-memory
         address-range
         address-range-lo
         address-range-hi
         address-readable?
         address-writable?
         memory-ref
         memory-set!
         heap-allocate-space!
         heap-free-space!

         debug-memory-section)

(require "debug.rkt"
         "sections.rkt"
         "utility.rkt")

;; Memory Representation
;;
;; We adopt some arbitrary conventions to simulate (in some sense) the typical
;; memory schema seen in x86-64 machines. This module provides an API through
;; which this representation can be built and used.
;;
;; The memory layout looks like this:
;;
;; #xfffffffffffffff8  +----------------+
;;                     |XXXXXXXXXXXXXXXX|
;;                     |XXXXXXXXXXXXXXXX|
;;                     |XXXXXXXXXXXXXXXX|
;;                     |XXXXXXXXXXXXXXXX|
;;                     |XXXXXXXXXXXXXXXX|
;; #x00007ffffffffff8  +================+
;;                     |                |
;;                     |     Stack      |
;;                     |                |
;;                     +~~~~~~~~~~~~~~~~+
;;                     //              //
;;                     +~~~~~~~~~~~~~~~~+
;;                     |                |
;;                     |      Heap      |
;;                     |                |
;;                     +----------------+
;;                     |      BSS       |
;;                     +----------------+
;;                     |      Data      |
;;                     +----------------+
;;                     |     Rodata     |
;;                     +----------------+
;;                     |      Text      |
;; #x0000000004000000  +================+
;;                     |XXXXXXXXXXXXXXXX|
;; #x0000000000000000  +----------------+
;;
;; Some addresses are rendered inaccessible to simulate kernel space or other
;; regions of memory that do not belong to the running program.
;;
;; The Text, Rodata, Data, and BSS sections are initialized to exactly as much
;; space as they need to store the program instructions, the read-only static
;; data, the initialized writable static data, and the uninitialized writable
;; static data, respectively.
;;
;; The Heap and Stack sections are initialized with zeroes. The Heap grows
;; upwards from the top of the last static section, and the Stack grows
;; downwards from the highest allowed address.
;;
;; Although x86 memory is byte-addressable, our simulation only handles words.
;; It is recommended that clients of this API align addresses to words to ensure
;; proper function.

;; The [Memory] struct abstracts over the names and addresses of sections. Its
;; purpose is to provide an opaque handle to the memory with which the user can
;; only interact by means of the API exposed from this module.
;;
;;   section-names    A list of the names of the sections defined in this
;;                    memory.
;;
;;   sections         A list of the [Section?]s defined in this memory. The
;;                    list's order is defined to be the same as [section-names].
;;
;;   ranges           A list of pairs representing the address ranges of the
;;                    sections defined in this memory. The list's order is
;;                    defined to be the same as [section-names].
(struct Memory (section-names sections ranges) #:transparent)

;; Converts a section name to an index used internally in [Memory?] structs.
(define (Memory-name->section-index memory section-name)
  (index-of (Memory-section-names memory) section-name))

;; Converts a section name to a [Section?].
(define (Memory-name->section memory section-name)
  (list-ref (Memory-sections memory)
            (Memory-name->section-index memory section-name)))

;; Converts a section name to an address range pair, where the low address is
;; the [car] of the pair and the high address is the [cdr].
(define (Memory-name->range memory section-name)
  (list-ref (Memory-ranges memory)
            (Memory-name->section-index memory section-name)))

;; Converts an address to an index used internally in [Memory?] structs.
(define (Memory-address->section-index memory address)
  (let ([address (align-address-to-word address)])
    (index-where (Memory-ranges memory)
                 (match-lambda [(list lo hi)
                                (and (>= address lo)
                                     (<= address hi))]))))

;; Converts an address to a section name.
(define (Memory-address->name memory address)
  (list-ref (Memory-section-names memory)
            (Memory-address->section-index memory address)))

;; Converts an address to a [Section?].
(define (Memory-address->section memory address)
  (list-ref (Memory-sections memory)
            (Memory-address->section-index memory address)))

(define (Memory-address->range memory address)
  (list-ref (Memory-ranges memory)
            (Memory-address->section-index memory address)))

;; The maximum number of words that the Heap can use. Defaults based on macOS
;; default.
(define max-heap-size (make-parameter (expt 2 11))) ;; TODO: Remove?

;; The initial quantity of words to allocate on the Stack. Default arbitrary.
;;
;; NOTE: All of Stack space will still be addressable. This parameter allows for
;; minor speed optimizations based on expected program Stack usage.
(define initial-stack-size (make-parameter (expt 2 8)))

;; The maximum number of words that the Stack can use. Default based on macOS
;; default.
(define max-stack-size (make-parameter (expt 2 20)))

;; The lowest usable address of program memory.
(define min-address (make-parameter #x0000000004000000))

;; The highest usable address of program memory.
(define max-address (make-parameter #x00007ffffffffff8))

;; Initializes a new memory representation, inserting the indicated values into
;; the static sections and zeroing-out the dynamic sections.
;;
;;   text-contents:       The contents of the Text section.
;;
;;                        Default: (list)
;;
;;   rodata-contents:     The contents of the Rodata section.
;;
;;                        Default: (list)
;;
;;   data-contents:       The contents of the Data section.
;;
;;                        Default: (list)
;;
;;   bss-contents:        The contents of the Bss section.
;;
;;                        Default: (list)
(define (make-memory #:text-contents   [text-contents   (list)]
                     #:rodata-contents [rodata-contents (list)]
                     #:data-contents   [data-contents   (list)]
                     #:bss-contents    [bss-contents    (list)])
  (let-values ([(next-address ranges->sections)
                (initialize-static-sections
                 (min-address)
                 (list (cons text text-contents)
                       (cons rodata rodata-contents)
                       (cons data data-contents)
                       (cons bss bss-contents)))])
    (let* ([heap-lo-address next-address]
           [heap-hi-address (+ heap-lo-address
                               (* word-size-bytes
                                  (max-heap-size)))]
           [heap-range (list heap-lo-address
                             heap-hi-address)]
           [heap-section (make-heap (max-heap-size))]
           [stack-hi-address (align-address-to-word (max-address))]
           [stack-lo-address (- stack-hi-address
                                (* word-size-bytes
                                   (sub1 (max-stack-size))))]
           [stack-range (list stack-lo-address
                              stack-hi-address)]
           [stack-section (make-stack (max-stack-size) (initial-stack-size))]
           [ranges->sections (cons (cons stack-range (cons stack stack-section))
                                   (cons (cons heap-range (cons heap heap-section))
                                         ranges->sections))])
      (call-with-values
       (thunk
        (for/fold ([names (list)]
                   [sections (list)]
                   [ranges (list)])
                  ([range-section-info ranges->sections])
          (match range-section-info
            [(cons section-range (cons section-name section))
             (values (cons section-name names)
                     (cons section sections)
                     (cons section-range ranges))])))
       Memory))))

;; Performs an address lookup, raising a ['segfault] error on failure. On
;; success, returns the internal index corresponding to the address.
(define (address-lookup memory address)
  (or (Memory-address->section-index memory address)
      (raise-user-error 'segfault "cannot access address ~v" address)))

;; Converts an address into a section name, a [Section?], and an offset
;; calculated to index into the [Section?] directly.
(define (address->section+offset memory address)
  (match-let* ([index (address-lookup memory address)]
               [section (list-ref (Memory-sections memory) index)]
               [name (list-ref (Memory-section-names memory) index)]
               [grows-down? (member name downward-sections)]
               [(list lo-address hi-address)
                (list-ref (Memory-ranges memory) index)]
               [offset-base (if grows-down? hi-address lo-address)]
               [offset (quotient (- (max offset-base address)
                                    (min offset-base address))
                                 word-size-bytes)])
    (values name section offset)))

;; Provides the pair of addresses that form the bounds of the indicated memory
;; section. The range returned is inclusive and word-aligned.
(define (address-range memory section-name)
  (Memory-name->range memory section-name))

;; Returns the lowest word-aligned address within the indicated section.
(define (address-range-lo memory section-name)
  (first (address-range memory section-name)))

;; Returns the highest word-aligned address within the indicated section.
(define (address-range-hi memory section-name)
  (second (address-range memory section-name)))

;; Determines whether an address is readable (i.e., the address corresponds to
;; any section in this memory, rather than lying outside the sections).
(define (address-readable? memory address)
  (and (Memory-address->section-index memory address)
       #t))

;; Determines whether an address is writable (i.e., the address lies within the
;; bounds of a read-write section).
(define (address-writable? memory address)
  (cond
    [(Memory-address->name memory address)
     => (λ (name) (member name read-write-sections))]))

;; Given a [Memory?] and address, looks up the current value stored at that
;; address in memory. Raises an error if the address cannot be accessed.
(define (memory-ref memory address)
  (let-values ([(_ section offset) (address->section+offset memory address)])
    (section-ref section offset)))

;; Given a [Memory?], address, time tick, and value, attempts to set the memory
;; accordingly. Raises an error if the address cannot be accessed, or if the
;; address belongs to a region of read-only memory.
(define (memory-set! memory address tick value)
  (let-values ([(name section offset) (address->section+offset memory address)])
    (unless (member name read-write-sections)
      (raise-user-error 'segfault "cannot write to address ~v in section ~a" address name))
    (section-set! section offset tick value)))

(define (debug-memory-section memory section-name)
  (when debug-on?
    (debug "  contents of section: ~a" (symbol->string section-name))
    (match (address-range memory section-name)
      [(list lo hi)
       (parameterize ([section-ref-failure-result #f])
         (for ([addr (if (member section-name downward-sections)
                         (in-range hi (sub1 lo) (* -1 word-size-bytes))
                         (in-range lo (add1 hi)       word-size-bytes))]
               #:break (not (memory-ref memory addr)))
           (debug "        ~a:\t~v"
                  (format-word addr 'hex)
                  (memory-ref memory addr))))])))