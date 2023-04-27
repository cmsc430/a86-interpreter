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

(require "../debug.rkt")

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

(define/debug (make-static-section contents)
  (StaticSection (list->cells contents)))

(define/debug (static-section-ref static-section offset)
  (vector-ref (StaticSection-contents static-section) offset))

(define/debug (static-section-set! static-section offset cell)
  (let ([contents (StaticSection-contents static-section)])
    (vector-set! contents
                 offset
                 (add-cell (vector-ref contents offset) cell))))

;;;; Heap

(struct Heap ([allocations #:mutable]  ;; [Listof [Pairof [Pairof lo-offset hi-offset]
                                       ;;                 [Vectorof words]]]
              [curr-size #:mutable]
              max-size))

(define/debug (Heap-offset->adjoff+contents heap offset)
  (for/or ([alloc (Heap-allocations heap)])
    (match alloc
      [(cons (cons lo-offset
                   hi-offset)
             contents)
       (and (>= offset lo-offset)
            (<= offset hi-offset)
            (cons (+ lo-offset offset) contents))])))

(define/debug (make-heap max-size [initial-allocation-size #f])
  (let ([heap (Heap '() 0 max-size)])
    (when initial-allocation-size
      (heap-allocate-space! heap initial-allocation-size))
    heap))

(define/debug (heap-ref heap offset)
  (let ([adjoff+contents (Heap-offset->adjoff+contents heap offset)])
    (if adjoff+contents
        (vector-ref (cdr adjoff+contents) (car adjoff+contents))
        #f)))

(define/debug (heap-set! heap offset cell)
  (let ([adjoff+contents (Heap-offset->adjoff+contents heap offset)])
    (if adjoff+contents
        (let ([adjusted-offset (car adjoff+contents)]
              [contents (cdr adjoff+contents)])
          (vector-set! contents
                       adjusted-offset
                       (add-cell (vector-ref contents adjusted-offset) cell)))
        (error 'segfault "no heap allocation for offset: ~v" offset))))

(define/debug (heap-allocate-space! heap new-allocation-size)
  (let ([curr-size (Heap-curr-size heap)]
        [max-size (Heap-max-size heap)])
    (when (> (+ new-allocation-size curr-size)
             max-size)
      (error 'segfault "cannot expand heap by ~v words" new-allocation-size))
    (let ([allocation-contents (make-vector new-allocation-size #f)]
          [new-size (+ curr-size new-allocation-size)])
      (set-Heap-allocations!
       heap
       (cons (cons (cons curr-size
                         (sub1 new-size))
                   allocation-contents)
             (Heap-allocations heap)))
      (set-Heap-curr-size! heap new-size))))

(define/debug (heap-free-space! heap base-offset)
  (for/list ([alloc (reverse (Heap-allocations heap))]
             #:unless (eq? base-offset (caar alloc)))
    alloc))

;;;; Stack

(struct Stack ([contents #:mutable]
               max-size))

(define/debug (make-stack max-size [initial-size 0])
  (Stack (make-vector initial-size #f) max-size))

(define/debug (stack-ref stack offset)
  (let ([contents (Stack-contents stack)])
    (if (< offset (vector-length contents))
        (vector-ref contents offset)
        #f)))

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

(define section-ref-failure-result (make-parameter 0))

(define/debug (section-ref section offset)
  (let* ([internal:section-ref
          (cond
            [(StaticSection? section) static-section-ref]
            [(Heap? section) heap-ref]
            [(Stack? section) stack-ref])]
         [result (internal:section-ref section offset)])
    (if result
        (Cell-value (vector-ref result 0))
        (section-ref-failure-result))))

(define/debug (section-set! section offset tick value)
  (let ([cell (Cell tick value)])
    (cond
      [(StaticSection? section)
       (static-section-set! section offset cell)]
      [(Heap? section)
       (heap-set! section offset cell)]
      [(Stack? section)
       (stack-set! section offset cell)])))
