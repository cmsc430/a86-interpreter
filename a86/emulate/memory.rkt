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

         in-memory-section

         debug-memory-section)

(require "../debug.rkt"
         "../utility.rkt"

         "sections.rkt")

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
(define/debug (Memory-name->section-index memory section-name)
  (index-of (Memory-section-names memory) section-name))

;; Converts a section name to a [Section?].
(define/debug (Memory-name->section memory section-name)
  (list-ref (Memory-sections memory)
            (Memory-name->section-index memory section-name)))

;; Converts a section name to an address range pair, where the low address is
;; the [car] of the pair and the high address is the [cdr].
(define/debug (Memory-name->range memory section-name)
  (list-ref (Memory-ranges memory)
            (Memory-name->section-index memory section-name)))

;; Converts an address to an index used internally in [Memory?] structs.
(define/debug (Memory-address->section-index memory address)
  (let ([address (align-address-to-word address)])
    (index-where (Memory-ranges memory)
                 (match-lambda [(list lo hi)
                                (and (>= address lo)
                                     (<= address hi))]))))

;; Converts an address to a section name.
(define/debug (Memory-address->name memory address)
  (list-ref (Memory-section-names memory)
            (Memory-address->section-index memory address)))

;; Converts an address to a [Section?].
(define/debug (Memory-address->section memory address)
  (list-ref (Memory-sections memory)
            (Memory-address->section-index memory address)))

(define/debug (Memory-address->range memory address)
  (list-ref (Memory-ranges memory)
            (Memory-address->section-index memory address)))

;; The initial quantity of words to allocate on the Heap. Default arbitrary.
;;
;; NOTE: Heap space beyond this amount will not be addressable. To use more of
;; the Heap section, you must allocate more space via [heap-allocate-space!].
(define initial-heap-size (make-parameter 10000))

;; The maximum number of words that the Heap can use. Defaults arbitrary.
(define max-heap-size (make-parameter (expt 2 17))) ;; TODO: Remove?

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
(define/debug (make-memory #:text-contents   [text-contents   (list)]
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
           [heap-section (make-heap (max-heap-size)
                                    (initial-heap-size))]
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
(define/debug (address-lookup memory address)
  (or (Memory-address->section-index memory address)
      (raise-user-error 'segfault "cannot access address ~v" address)))

;; Converts an address into a section name, a [Section?], and an offset
;; calculated to index into the [Section?] directly.
(define/debug (address->section+offset memory address)
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
(define/debug (address-range memory section-name)
  (Memory-name->range memory section-name))

;; Returns the lowest word-aligned address within the indicated section.
(define/debug (address-range-lo memory section-name)
  (first (address-range memory section-name)))

;; Returns the highest word-aligned address within the indicated section.
(define/debug (address-range-hi memory section-name)
  (second (address-range memory section-name)))

;; Determines whether an address is readable (i.e., the address corresponds to
;; any section in this memory, rather than lying outside the sections).
(define/debug (address-readable? memory address)
  (and (Memory-address->section-index memory address)
       #t))

;; Determines whether an address is writable (i.e., the address lies within the
;; bounds of a read-write section).
(define/debug (address-writable? memory address)
  (cond
    [(Memory-address->name memory address)
     => (λ (name) (member name read-write-sections))]))

;; Given a [Memory?] and address, looks up the current value stored at that
;; address in memory. Raises an error if the address cannot be accessed.
(define/debug (memory-ref memory address)
  (let-values ([(_ section offset) (address->section+offset memory address)]
               [(byte-offset) (remainder address word-size-bytes)])
    (if (zero? byte-offset)
        ;; If word-aligned, just get the word.
        (section-ref section offset)
        ;; Otherwise, get this word and the next one and combine the results
        ;; appropriately.
        ;;
        ;; +----------- 0x0010 : Base address of lesser word.
        ;; |        +-- 0x0018 : Base address of greater word.
        ;; |        |
        ;; V        V
        ;; +--------+--------+
        ;; |#####$$$|$$$$$###| <-- Original bits with desired word split across
        ;; +--------+--------+     the word boundary.
        ;;       ^
        ;;       |
        ;;       +-- Byte offset at which desired word begins.
        ;;
        ;; +--------+ +--------+
        ;; |00000111| |11111000| <-- Masks for desired values.
        ;; +--------+ +--------+
        ;;
        ;; +--------+ +--------+
        ;; |00000$$$| |$$$$$000| <-- Masked desired values.
        ;; +--------+ +--------+
        ;;
        ;; +--------+ +--------+
        ;; |$$$00000| |000$$$$$| <-- Shifted desired values.
        ;; +--------+ +--------+
        ;;
        ;; +--------+
        ;; |$$$$$$$$| <-- End result.
        ;; +--------+
        (let* ([word0 (section-ref section offset)]
               [word1 (section-ref section (add1 offset))]
               [mask0 (make-mask (* -8 (- word-size-bytes byte-offset)))]
               [mask1 (make-mask (* 8 byte-offset))]
               [value0 (arithmetic-shift (bitwise-and mask0 word0)
                                         (* -8 byte-offset))]
               [value1 (arithmetic-shift (bitwise-and mask1 word1)
                                         (* 8 (- word-size-bytes byte-offset)))]
               [value (bitwise-ior value0 value1)])
          value))))

;; Given a [Memory?], address, time tick, and value, attempts to set the memory
;; accordingly. Raises an error if the address cannot be accessed, or if the
;; address belongs to a region of read-only memory.
(define/debug (memory-set! memory address tick value [byte-count word-size-bytes])
  (let-values ([(name section offset) (address->section+offset memory address)]
               [(byte-offset) (remainder address word-size-bytes)])
    (unless (member name read-write-sections)
      (raise-user-error 'segfault "cannot write to address ~v in section ~a" address name))
    (unless (<= byte-count word-size-bytes)
      (error 'memory-set! "expected byte-count less than ~a; got ~a" word-size-bytes byte-count))
    (cond
      ;; If writing a word-aligned word, take the easy path.
      [(and (zero? byte-offset)
            (= byte-count word-size-bytes))
       (section-set! section offset tick value)]
      ;; If writing less than a word that will not cross a word boundary.
      ;;
      ;; +-- 0x0010 : Base address of word.
      ;; |
      ;; V
      ;; +--------+
      ;; |########| <-- Original bits.
      ;; +--------+
      ;;      ^
      ;;      |
      ;;      +-- Byte offset at which new bits will be written.
      ;;
      ;; +---+
      ;; |$$$| <-- New bits to write (possibly not full word).
      ;; +---+
      ;;
      ;; +--------+
      ;; |11110001| <-- Mask for existing bits to keep.
      ;; +--------+
      ;;
      ;; +--------+
      ;; |0000$$$0| <-- Shifted new bits.
      ;; +--------+
      ;;
      ;; +--------+
      ;; |####000#| <-- Masked original bits.
      ;; +--------+
      ;;
      ;; +--------+
      ;; |####$$$#| <-- End result.
      ;; +--------+
      [(<= (+ byte-offset byte-count)
           word-size-bytes)
       (let* ([mask-010 (make-mask (* 8 byte-count)
                                   word-size-bits
                                   (* 8 byte-offset))]
              [mask-101 (bitwise-xor (make-mask word-size-bits)
                                     mask-010)]
              [word0 (bitwise-and mask-101
                                  (section-ref section offset))]
              [value0 (arithmetic-shift value (* 8 byte-offset))]
              [new-word0 (bitwise-ior word0 value0)])
         (section-set! section offset tick new-word0))]
      ;; Otherwise (writing a word or less that will cross a word boundary).
      ;;
      ;; +----------- 0x0010 : Base address of lesser word.
      ;; |        +-- 0x0018 : Base address of greater word.
      ;; |        |
      ;; V        V
      ;; +--------+--------+
      ;; |########|########| <-- Original bits over two words.
      ;; +--------+--------+
      ;;        ^
      ;;        |
      ;;        +-- Byte offset at which new bits will be written.
      ;;
      ;; +-----+
      ;; |$$$$$| <-- New bits to write (possibly not full word).
      ;; +-----+
      ;;
      ;; +--------+ +--------+
      ;; |11111100| |00011111| <-- Masks for existing values to keep.
      ;; +--------+ +--------+
      ;;
      ;; +--------+ +--------+
      ;; |######00| |000#####| <-- Masked values to keep.
      ;; +--------+ +--------+
      ;;
      ;; +-----+ +-----+
      ;; |11000| |00111| <-- Masks for separating new bits.
      ;; +-----+ +-----+
      ;;
      ;; +--------+ +--------+
      ;; |000000$$| |$$$00000| <-- Masked-and-shifted new bits.
      ;; +--------+ +--------+
      ;;
      ;; +--------+--------+
      ;; |######$$|$$$#####| <-- End result.
      ;; +--------+--------+
      [else
       (let* ([byte-count0 (- word-size-bytes byte-offset)]
              [byte-count1 (- byte-count byte-count0)]
              [mask0 (make-mask (* 8 byte-offset))]
              [mask1 (make-mask (* -8 (- word-size-bytes byte-count1)))]
              [word0 (bitwise-and mask0 (section-ref section offset))]
              [word1 (bitwise-and mask1 (section-ref section (add1 offset)))]
              [vmask0 (make-mask (* 8 byte-count0)
                                 (* 8 byte-count))]
              [vmask1 (make-mask (* -8 byte-count1)
                                 (* 8 byte-count))]
              [value0 (arithmetic-shift (bitwise-and vmask0 value)
                                        (* 8 byte-offset))]
              [value1 (arithmetic-shift (bitwise-and vmask1 value)
                                        (* -8 byte-count0))]
              [new-word0 (bitwise-ior word0 value0)]
              [new-word1 (bitwise-ior word1 value1)])
         (section-set! section offset tick new-word0)
         (section-set! section (add1 offset) tick new-word1))])))

;; A stream for iterating over the contents of the indicated section. Each step
;; produces a pair containing the address and the contents of memory at that
;; address.
;;
;; The [order] can be specified as one of the following:
;;
;;   'default       The order specified by the section's definition.
;;   'reversed      The opposite of the result of ['default] ordering.
;;   'ascending     From the low address to the high address.
;;   'descending    From the high address to the low address.
;;
;; If [exit-early?] is not [#f], iteration will prematurely halt when an address
;; is checked that has not been written to. Otherwise, iteration will continue
;; over the entire section.
(define (in-memory-section memory
                           section-name
                           #:order [order 'default]
                           #:exit-early? [exit-early? #f])
  (let* ([hi-addr (address-range-hi memory section-name)]
         [lo-addr (address-range-lo memory section-name)]
         [step (cond
                 [(or (eq? order 'ascending)
                      (and (eq? order 'default)
                           (memq section-name upward-sections))
                      (and (eq? order 'reversed)
                           (memq section-name downward-sections)))
                  8]
                 [(or (eq? order 'descending)
                      (and (eq? order 'default)
                           (memq section-name downward-sections))
                      (and (eq? order 'reversed)
                           (memq section-name upward-sections)))
                  -8])]
         [from-addr (if (positive? step)
                        lo-addr
                        hi-addr)]
         [to-addr (if (positive? step)
                      hi-addr
                      lo-addr)])
    (parameterize ([section-ref-failure-result (if exit-early?
                                                   #f
                                                   (section-ref-failure-result))])
      (for/stream ([address (in-inclusive-range from-addr to-addr step)]
                   #:break (and exit-early?
                                (eq? #f (memory-ref memory address))))
        (cons address
              (memory-ref memory address))))))

(define (debug-memory-section memory section-name)
  (begin/debug
    ("contents of section: ~a" (symbol->string section-name))
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
