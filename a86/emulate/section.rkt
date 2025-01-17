#lang racket

(provide handling-strategy-limited
         handling-strategy-rotating
         handling-strategy-unlimited
         handling-strategy
         max-cell-depth
         make-static-section
         make-heap
         make-stack
         section-ref
         section-ref-failure-result
         section-set!
         heap-allocate-space!
         heap-free-space!)

(require "../debug.rkt"

         "exn.rkt")

;; A pair of a time tick with a value. These are used for keeping track of
;; "when" a value was introduced to the machine.
(struct Cell (tick value))

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

;; Given a list of elements, produces a vector of [Cell?]s of the same length.
(define/debug (list->cells contents [time-tick 0])
  (for/vector ([element contents])
    (vector-immutable (Cell time-tick element))))

;; Adds a new cell to a vector of cells according to the handling strategy.
(define/debug (add-cell cells cell)
  (let ([max-depth (max-cell-depth)]
        [handling (handling-strategy)])
    (match cells
      ;; When there are no cells, we create a new cells vector.
      [#f (vector-immutable cell)]
      ;; When there is at least one cell, the [max-cell-depth] has been reached,
      ;; and we're using a ['rotating] [handling-strategy], we drop the oldest
      ;; cell to write the new one.
      [(vector cells ... _last-cell)
       #:when (and (eq? handling handling-strategy-rotating)
                   (>= (add1 (length cells)) max-depth))
       (apply vector-immutable cell cells)]
      ;; When the [handling-strategy] is ['limited] and the [max-cell-depth] has
      ;; been reached, an error is raised.
      [(vector cells ...)
       #:when (and (eq? handling handling-strategy-limited)
                   (>= (length cells) max-depth))
       (error 'add-cell "maximum cell depth reached")]
      ;; In all other cases, the new cell is put at the front of a new vector.
      [(vector cells ...)
       (apply vector-immutable cell cells)])))

;;;; Statically sized sections

(struct StaticSection ([contents #:mutable]))

;; Creates a new [StaticSection?] containing all the indicating contents as
;; cells with a time-tick of 0.
(define/debug (make-static-section contents)
  (StaticSection (list->cells contents)))

;; Returns a vector containing the [Cell?]s corresponding to the offset.
(define/debug (static-section-ref static-section offset)
  (vector-ref (StaticSection-contents static-section) offset))

;; Adds the given [Cell?] at the appropriate offset.
(define/debug (static-section-set! static-section offset cell)
  (let ([contents (StaticSection-contents static-section)])
    (vector-set! contents
                 offset
                 (add-cell (vector-ref contents offset) cell))))

;;;; Heap

;; The heap is more intricate than other sections because it can be dynamically
;; extended as needed. Each such extension is called an "allocation". The
;; internal structure used for an allocation is a pair containing the pair of
;; offset boundaries (inclusive) and a vector corresponding to the words in
;; memory.
(struct Heap ([allocations #:mutable]  ;; [Listof [Pairof [Pairof lo-offset
                                       ;;                         hi-offset]
                                       ;;                 [Vectorof words]]]
              [curr-size #:mutable]
              max-size))

;; To index into the heap requires decoding which allocation the offset will be
;; found in. If the given offset corresponds to a word within an allocation in
;; this heap, the returned result will be a pair of the adjusted offset with the
;; contents of the allocation.
(define/debug (Heap-offset->adjoff+contents heap offset)
  (for/or ([alloc (Heap-allocations heap)])
    (match alloc
      [(cons (cons lo-offset
                   hi-offset)
             contents)
       (and (>= offset lo-offset)
            (<= offset hi-offset)
            (cons (+ lo-offset offset) contents))])))

;; Initializes a [Heap?], optionally with an initial allocation.
(define/debug (make-heap max-size [initial-allocation-size #f])
  (let ([heap (Heap '() 0 max-size)])
    (when initial-allocation-size
      (void (heap-allocate-space! heap initial-allocation-size)))
    heap))

;; Returns a vector containing the [Cell?]s corresponding to the offset.
(define/debug (heap-ref heap offset)
  (let ([adjoff+contents (Heap-offset->adjoff+contents heap offset)])
    (if adjoff+contents
        (vector-ref (cdr adjoff+contents) (car adjoff+contents))
        #f)))

;; Adds the given [Cell?] at the appropriate offset within the appropriate
;; allocation.
(define/debug (heap-set! heap offset cell)
  (let ([adjoff+contents (Heap-offset->adjoff+contents heap offset)])
    (if adjoff+contents
        (let ([adjusted-offset (car adjoff+contents)]
              [contents (cdr adjoff+contents)])
          (vector-set! contents
                       adjusted-offset
                       (add-cell (vector-ref contents adjusted-offset) cell)))
        (raise-a86-emulator-segfault-error "no heap allocation for offset: ~v" offset))))

;; Extends the heap by allocating [new-allocation-size] more memory. The size is
;; given in words. Returns the lowest address of the new region.
(define/debug (heap-allocate-space! heap new-allocation-size)
  (let ([curr-size (Heap-curr-size heap)]
        [max-size (Heap-max-size heap)])
    (when (> (+ new-allocation-size curr-size)
             max-size)
      (raise-a86-emulator-segfault-error 'heap-allocate-space! "cannot expand heap by ~v words" new-allocation-size))
    (let ([allocation-contents (make-vector new-allocation-size #f)]
          [new-size (+ curr-size new-allocation-size)])
      (set-Heap-allocations!
       heap
       (cons (cons (cons curr-size
                         (sub1 new-size))
                   allocation-contents)
             (Heap-allocations heap)))
      (set-Heap-curr-size! heap new-size)
      curr-size)))

;; Removes the allocation whose low offset is equal to the given offset. If no
;; allocation is freed, there is no effect on the internal memory and no error
;; is raised.
(define/debug (heap-free-space! heap base-offset)
  (for/list ([alloc (reverse (Heap-allocations heap))]
             #:unless (eq? base-offset (caar alloc)))
    alloc))

;;;; Stack

(struct Stack ([contents #:mutable]
               max-size))

;; Initializes a [Stack?].
(define/debug (make-stack max-size [initial-size 0])
  (Stack (make-vector initial-size #f) max-size))

;; Returns a vector containing the [Cell?]s corresponding to the offset.
(define/debug (stack-ref stack offset)
  (let ([contents (Stack-contents stack)])
    (if (< offset (vector-length contents))
        (vector-ref contents offset)
        #f)))

;; Adds the given [Cell?] at the appropriate offset.
(define/debug (stack-set! stack offset cell)
  (let* ([contents (Stack-contents stack)]
         [curr-size (vector-length contents)])
    (when (>= offset curr-size)
      (let* ([new-size (min (let next-size ([size curr-size])
                              (if (>= offset size)
                                  (next-size (* 2 size))
                                  size))
                            (Stack-max-size stack))]
             [new-contents (make-vector new-size #f)])
        (vector-copy! new-contents 0 contents)
        (set-Stack-contents! stack new-contents)
        (set! contents new-contents)))
    (vector-set! contents
                 offset
                 (add-cell (vector-ref contents offset) cell))))

;;;; Generalized section methods.

;; The default value to return if the lookup is unsuccessful.
(define section-ref-failure-result (make-parameter 0))

;; Finds the most recent value corresponding to the indicated tick. This assumes
;; the [cells] are written in reverse-chronological order (i.e., newest first).
(define/debug (retrieve-value cells tick)
  (for/or ([cell (in-vector cells)])
    (match cell
      [(Cell t v) (and (<= t tick)
                       v)])))

;; Returns the value corresponding to the offset and tick. If [tick] is [#f]
;; (the default), the most recent value is returned. If no value can be found,
;; the result of [(section-ref-failure-result)] is used.
(define/debug (section-ref section offset [tick #f])
  (let* ([internal:section-ref
          (cond
            [(StaticSection? section) static-section-ref]
            [(Heap? section) heap-ref]
            [(Stack? section) stack-ref])]
         [cells (internal:section-ref section offset)])
    (or (and cells
             (retrieve-value cells (or tick +inf.f)))
        (section-ref-failure-result))
    #;(if cells
        (retrieve-value cells (or tick +inf.f))
        (section-ref-failure-result))))

;; Records the indicated value at the appropriate offset in the given section.
(define/debug (section-set! section offset tick value)
  (let ([cell (Cell tick value)])
    (cond
      [(StaticSection? section)
       (static-section-set! section offset cell)]
      [(Heap? section)
       (heap-set! section offset cell)]
      [(Stack? section)
       (stack-set! section offset cell)])))
