#lang racket

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

(provide text rodata data bss heap stack
         read-only-sections
         read-write-sections
         static-sections
         dynamic-sections
         downward-sections
         upward-sections
         handling-strategy-limited
         handling-strategy-rotating
         handling-strategy-unlimited
         handling-strategy
         max-cell-depth
         make-memory
         memory-ref
         memory-set!
         section->range
         debug-instructions)

(module sections racket
  ;; A section is a light wrapper around a vector. Sections have a maximum size,
  ;; but their internal vectors can be smaller than that. Writes to indices
  ;; outside the bounds of the internal vector but within the maximum size
  ;; result in a resizing of the internal vector.
  ;;
  ;; Sections also handle logic related to the specific internal memory model we
  ;; adapt for the interpreter. Specifically, we want interpreter memory to have
  ;; memory itself, i.e., we would like to be able to search backward through
  ;; the write history of an address to aid in debugging. To support this,
  ;; insertions into sections record an interpreter time-tick associated with
  ;; the write. A given memory address will have record of its past writes and
  ;; their time ticks.
  ;;
  ;; However, storing arbitrarily many such time tick/value pairs could pose a
  ;; problem of efficiency. Sectors therefore support three different memory
  ;; handling strategies to address this:
  ;;
  ;;   limited    each address in writable memory can be written to only so many
  ;;              times, after which an error is raised
  ;;
  ;;   rotating   each address in writable memory can be written to only so many
  ;;              times, after which the oldest values are dropped in favor of
  ;;              keeping the newer values
  ;;
  ;;   unlimited  no bound on the amount of times a specific address can be
  ;;              written to
  ;;
  ;; The default handling strategy is [rotating], with a default maximum memory
  ;; depth of [10] cells.

  (provide handling-strategy-limited
           handling-strategy-rotating
           handling-strategy-unlimited
           handling-strategy
           max-cell-depth
           make-section
           section-ref
           section-set!)

  ;; Memory depth is limited; when exhausted, errors are raised.
  (define handling-strategy-limited 'limited)
  ;; Memory depth is limited; when exhausted, oldest values are dropped.
  (define handling-strategy-rotating 'rotating)
  ;; No limit on the depth of memory.
  (define handling-strategy-unlimited 'unlimited)
  ;; The current memory handling strategy.
  (define handling-strategy (make-parameter handling-strategy-rotating))
  ;; The maximum cell depth.
  (define max-cell-depth (make-parameter 10))

  ;; A pair of a time tick with a value. These are used for keeping track of
  ;; "when" a value was introduced to the machine.
  (struct Cell (tick value))

  ;; A section simply contains a vector of vectors of cells, and has a maximum
  ;; supported size. The contents vector is automatically resized to accommodate
  ;; writes that need it, up to the maximum size.
  (struct Section ([contents #:mutable]
                   max-size))

  ;; Initializes a section from a given list of [contents]. The maximum size
  ;; specifies how large the section may be. If the [max-size] is left as [#f],
  ;; the section is only allowed to be as large as its initial [contents].
  (define (make-section contents [max-size #f])
    (Section (if (vector? contents)
                 contents
                 (apply vector (map (λ (v) (vector-immutable (Cell 0 v))) contents)))
             (or max-size (length contents))))

  (define (internal:section-ref contents index)
    (and (< index (vector-length contents))
         (vector-ref contents index)))

  ;; Accesses an index within a section. It is assumed that the index lies
  ;; within the section's maximum bounds. If the access fails, [failure-result]
  ;; is applied (if it is a procedure) or returned (if it is not).
  ;;
  ;; NOTE: [section-ref] is part of the external API for sections, so it
  ;; retrieves the most recently stored value at the indicated index.
  (define (section-ref section index [failure-result 0])
    (let* ([contents (Section-contents section)]
           [cells (internal:section-ref contents index)])
      (if cells
          (Cell-value (vector-ref cells 0))
          (if (procedure? failure-result)
              (failure-result)
              failure-result))))

  (define (internal:section-set! section index cell)
    (let* ([contents (Section-contents section)]
           [curr-size (vector-length contents)])
      (if (>= index curr-size)
          (let ([new-size (min (let next-size ([size curr-size])
                                 (if (>= index size)
                                     (next-size (* 2 size))
                                     size))
                               (Section-max-size section))])
            (if (>= index new-size)
                (error 'section-set!
                       "index beyond maximum section size: ~v"
                       index)
                (let ([new-contents (make-vector new-size #f)])
                  (vector-copy! new-contents
                                0
                                contents)
                  (set-Section-contents! section new-contents)))
            (internal:section-set! section index cell))
          (let ([max-depth (max-cell-depth)]
                [handling (handling-strategy)])
            (match (internal:section-ref contents index)
              [#f (vector-set! contents index (vector-immutable cell))]
              [(vector cells ...)
               #:when (and (>= (length cells) max-depth)
                           (eq? handling handling-strategy-limited))
               (error 'section-set!
                      "index ~v cannot be written to more than ~v times"
                      index
                      max-depth)]
              [(vector cells ... _last-cell)
               #:when (and (>= (add1 (length cells)) max-depth)
                           (eq? handling handling-strategy-rotating))
               (vector-set! contents index
                            (apply vector-immutable
                                   cell
                                   cells))]
              [(vector cells ...)
               (vector-set! contents index
                            (apply vector-immutable
                                   cell
                                   cells))])))))

  ;; Writes to an index within a section. It is assumed that the index lies
  ;; within the section's maximum bounds.
  ;;
  ;; If the index lies outside the internal [contents] vector, the [contents]
  ;; will be resized to accommodate, up to the maximum size allowed.
  (define (section-set! section index tick value)
    (internal:section-set! section index (Cell tick value))))

(require 'sections
         "debug.rkt"
         "utility.rkt")

;; The memory struct abstracts over the address ranges and sections.
;;
;;   ranges->sections:  An association list mapping address ranges to name-
;;                      section pairs. An example might be:
;;
;;                      (list (list (cons    0    8)  'text #<Section>)
;;                            (list (cons   16   48)  'data #<Section>)
;;                            (list (cons   56  856)  'heap #<Section>)
;;                            (list (cons 1920 2040) 'stack #<Section>))
(struct Memory (ranges->sections) #:transparent)

;; These definitions for the section names simply protect against typos when
;; writing symbols, but we also establish some conventions around the use of
;; each section:
;;
;;                  text  rodata  data  bss  heap  stack
;;                +--------------------------------------
;;   writable?    |                x     x    x     x
;;   static?      |  x     x       x     x
;;   grows down?  |                                 x
;;
;; The below defined lists can be used to dynamically query a given section for
;; its intended use.
(define-values (                   text  rodata  data  bss  heap  stack )
  (values                         'text 'rodata 'data 'bss 'heap 'stack ))
(define read-only-sections  (list  text  rodata                         ))
(define read-write-sections (list                data  bss  heap  stack ))
(define static-sections     (list  text  rodata  data  bss              ))
(define dynamic-sections    (list                           heap  stack ))
(define downward-sections   (list                                 stack ))
(define upward-sections     (list  text  rodata  data  bss  heap        ))

;; Sets up the static sections, which are allocated only enough space to
;; accommodate their initial contents. Each section is set in memory directly
;; above the section before it, beginning at the [lo-address].
;;
;; Returns two values: the next low address that can be used outside of the
;; allocated sections, and an association list mapping address ranges to a pair
;; of the section's name and its corresponding [Section?].
(define (initialize-static-sections lo-address content-pairs)
  (for/fold ([lo-address lo-address]
             [ranges->sections (list)])
            ([content-pair content-pairs])
    (match content-pair
      [(cons name contents)
       (if (not (empty? contents))
           (let* ([hi-address (+ lo-address
                                 (* word-size-bytes
                                    (sub1 (length contents))))]
                  [section (make-section contents)]
                  [address-range (cons lo-address hi-address)])
             (values (greater-word-aligned-address hi-address)
                     (cons (cons address-range (cons name section))
                           ranges->sections)))
           (values lo-address
                   ranges->sections))])))

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
;;
;;   initial-heap-size:   The initial quantity of words to allocate on the
;;                        Heap. Note that all of Heap space will still be
;;                        addressable; this is to allow for minor speed
;;                        optimizations based on expected program Heap usage.
;;
;;                        Default: 2^8 words (arbitrary)
;;
;;   max-heap-size:       The maximum number of words that the Heap can use.
;;
;;                        Default: 2^11 words (based on macOS default)
;;
;;   initial-stack-size:  The initial quantity of words to allocate on the
;;                        Stack. Note that all of Stack space will still be
;;                        addressable; this is to allow for minor speed
;;                        optimizations based on expected program Stack usage.
;;
;;                        Default: 2^8 words (arbitrary)
;;
;;   max-stack-size:      The maximum number of words that the Stack can use.
;;
;;                        Default: 2^20 words (based on macOS default)
;;
;;   min-address:         The lowest usable address of program memory.
;;
;;                        Default: #x0000000004000000
;;
;;   max-address:         The highest usable address of program memory.
;;
;;                        Default: #x00007ffffffffff8
(define (make-memory #:text-contents [text-contents (list)]
                     #:rodata-contents [rodata-contents (list)]
                     #:data-contents [data-contents (list)]
                     #:bss-contents [bss-contents (list)]
                     #:initial-heap-size [initial-heap-size (expt 2 8)]
                     #:max-heap-size [max-heap-size (expt 2 11)]
                     #:initial-stack-size [initial-stack-size (expt 2 8)]
                     #:max-stack-size [max-stack-size (expt 2 20)]
                     #:min-address [min-address #x0000000004000000]
                     #:max-address [max-address #x00007ffffffffff8])
  (let-values ([(next-address ranges->sections)
                (initialize-static-sections
                 min-address
                 (list (cons text text-contents)
                       (cons rodata rodata-contents)
                       (cons data data-contents)
                       (cons bss bss-contents)))])
    (let* ([heap-lo-address next-address]
           [heap-hi-address (+ heap-lo-address
                               (* word-size-bytes
                                  (sub1 max-heap-size)))]
           [heap-range (cons heap-lo-address
                             heap-hi-address)]
           [heap-section (make-section (make-list initial-heap-size 0)
                                       max-heap-size)]
           [stack-hi-address (align-address-to-word max-address)]
           [stack-lo-address (- stack-hi-address
                                (* word-size-bytes
                                   (sub1 max-stack-size)))]
           [stack-range (cons stack-lo-address
                              stack-hi-address)]
           [stack-section (make-section (make-list initial-stack-size 0)
                                        max-stack-size)]
           [ranges->sections (reverse (cons (cons stack-range (cons stack stack-section))
                                            (cons (cons heap-range (cons heap heap-section))
                                                  ranges->sections)))])
      (Memory ranges->sections))))

;; Converts an address into a section and index. If the address does not
;; correspond to a region of memory governed by a defined section, returns [#f].
;;
;; Returns a three-element list containing the name of the section, the
;; [Section?] struct, and the index into that [Section?]'s contents
;; corresponding to the given address.
(define (address->section+index memory address)
  (match (assoc (align-address-to-word address)
                (Memory-ranges->sections memory)
                (λ (address section-range)
                  (and (>= address (car section-range))
                       (<= address (cdr section-range)))))
    [#f #f]
    [(cons (cons lo-address hi-address) (cons section-name section))
     (let* ([grows-down? (member section-name downward-sections)]
            [index-base (if grows-down? hi-address lo-address)]
            [index (quotient (- (max index-base address)
                                (min index-base address))
                             word-size-bytes)])
       (list section-name section index))]))

;; Performs an address lookup, raising an error (labeled ['segfault]) on
;; failure. On success, returns the result of [address->section+index].
(define (address-lookup memory address)
  (match (address->section+index memory address)
    [#f (raise-user-error 'segfault "cannot access address ~v" address)]
    [v v]))

;; Given a [Memory?] and address, looks up the current value stored at that
;; address in memory. Raises an error if the address cannot be accessed.
(define (memory-ref memory address)
  (match (address-lookup memory address)
    [(list _ section index)
     (section-ref section index)]))

;; Given a [Memory?], address, time tick, and value, attempts to set the memory
;; accordingly. Raises an error if the address cannot be accessed, or if the
;; address belongs to a region of read-only memory.
(define (memory-set! memory address tick value)
  (match (address-lookup memory address)
    [(list name section index)
     #:when (member name read-write-sections)
     (section-set! section index tick value)]
    [(cons name _)
     (raise-user-error 'segfault "cannot write to address ~v in section ~a" address name)]))

;; Given a [Memory?] and the name of a section, retrieves the corresponding
;; address range.
(define (section->range memory section-name)
  (cond
    [(findf (λ (section-info)
              (eq? section-name (cadr section-info)))
            (Memory-ranges->sections memory))
     => (λ (info) (car info))]
    [else (raise-user-error 'section->range "undefined section: ~v" section-name)]))

;; Prints the contents of the .text section with addresses.
(define (debug-instructions memory)
  (when debug-on?
    (debug "contents of .text:")
    (let* ([text-range (section->range memory text)]
           [lo-address (car text-range)]
           [hi-address (cdr text-range)])
      (for ([address (in-range hi-address (lesser-word-aligned-address lo-address) (* -1 word-size-bytes))])
        (debug "  ~a\t~v" (format-word address 'hex) (memory-ref memory address))))))
