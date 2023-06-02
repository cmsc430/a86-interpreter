#lang racket

(provide fresh-region-state
         current-region-state
         regions:redraw-all!
         regions:selected
         regions:select!
         regions:select-next!
         regions:select-prev!
         define-region-installer
         regions:install
         regions:find-empty-rectangle)

(require "region.rkt"
         "term.rkt"

         (for-syntax syntax/parse))

;; TODO: If I abstracted out the automatically resizing vectors from other parts
;; of the code, they could be used here as well... hm.
(struct region-state
  (regions             ;; [Listof region?]
   region-index)
  #:mutable)

(define (fresh-region-state) (region-state (list) #f))

(define current-region-state (make-parameter #f))

(define (regions:redraw-all!)
  (for ([r (in-list (current-regions))])
    (call r 'redraw!)))

(define (current-regions)
  (region-state-regions (current-region-state)))

(define (current-region-index)
  (region-state-region-index (current-region-state)))

(define (regions:selected)
  (list-ref (current-regions)
            (current-region-index)))

(define (regions:select! selector)
  (let* ([regions (current-regions)]
         [region-count (length regions)])
    (match selector
      [(? integer?)
       (set-region-state-region-index!
        (current-region-state)
        (modulo selector region-count))]
      [(or (and 'next (app (λ (_) add1) func))
           (and 'prev (app (λ (_) sub1) func)))
       (let* ([old-idx (current-region-index)]
              [new-idx (modulo (func old-idx) region-count)])
         (set-region-state-region-index! (current-region-state) new-idx))])))

(define (regions:select-next!) (regions:select! 'next))

(define (regions:select-prev!) (regions:select! 'prev))

(define-syntax (define-region-installer stx)
  (syntax-parse stx
    [(_ (id:id) name width-spec height-spec make-refresh)
     #'(define (id)
         (regions:install (make-region name make-refresh)
                          width-spec
                          height-spec))]))

(define (regions:install region)
  (set-region-state-regions! (current-region-state)
                             (append (current-regions) (list region))))

;; Attempts to install a new region in the state by finding an empty space with
;; a specified minimum size.
;;
;; The first parameter is a function that takes the x- and y-coordinates of the
;; top-left and bottom-right corners of the space found, i.e., it will be called
;; as [(make-region-func min-x min-y max-x max-y)].
;;
;; The second and third parameters describe the number of columns and rows
;; required to initialize this region, respectively. These can be given as exact
;; positive integers, but they can also be given as pairs with additional
;; information:
;;
;;   * [(cons 'per n)] or [(cons 'percent n)] will treat the number [n] as a
;;     percentage of the screen rather than an exact number of columns or rows.
;;     This can be useful for proportional declarations or needing to find a
;;     space for a full-width region.
;;   * [(cons 'exact n)] is the same as providing an exact integer.
#;(define (regions:install make-region-func width-spec height-spec)
  (define (convert-input-num input-num max-size)
    (match input-num
      [(or (? exact-nonnegative-integer? n)
           (cons 'exact
                 (? exact-nonnegative-integer? n)))
       (if (<= n max-size)
           n
           #f)]
      [(cons (or 'per 'percent 'percentage)
             (or (? (λ (n) (and (inexact? n)
                                (<= 0.0 n 1.0)))
                    n)
                 (? (λ (n) (and (exact-nonnegative-integer? n)
                                (<= 0 n 100)))
                    (app (λ (n) (/ n 100)) n))))
       (inexact->exact (round (* n max-size)))]))
  (let-values ([(screen-cols screen-rows) (term:screen-size)])
    (let ([num-cols (or (convert-input-num width-spec screen-cols)
                        (error 'regions:install
                               "column size not within screen bounds: ~a"
                               width-spec))]
          [num-rows (or (convert-input-num height-spec screen-rows)
                        (error 'regions:install
                               "row size not within screen bounds: ~a"
                               height-spec))])
      (match (find-empty-rectangle num-cols num-rows)
        [#f (error 'regions:install "unable to find empty space for new region")]
        [(list min-x min-y max-x max-y)
         (let ([region (make-region-func min-x min-y max-x max-y)])
           (set-region-state-regions!
            (current-region-state)
             (append (current-regions) (list region))))]))))

;; Searches for an empty rectangle of the specified size, starting from the
;; indicated start position. Returns [#f] if no such rectangle exists.
(define (regions:find-empty-rectangle test-cols test-rows)
  ;; FIXME: Doesn't check space in-between!
  (for/or ([(x0 y0) (in-term)]
           #:unless (regions:at-pos x0 y0)
           #:do [(define prev-xf #f)])
    (for/or ([yf (in-col y0 #:reversed? #t)]
             #:break (regions:at-pos x0 yf))
      (for/or ([xf (in-row x0 prev-xf #:reversed? #t)]
               #:break (regions:at-pos xf yf)
               #:do [(set! prev-xf xf)])
        (let ([width (test-cols (add1 (- xf x0)))]
              [height (test-rows (add1 (- yf y0)))])
          (and width
               height
               (list x0 y0 (sub1 (+ x0 width)) (sub1 (+ y0 height))))))))

  ;; TODO: COPY THESE COMMENTS
  #;(for/or ([(x0 y0) (in-term)]
           ;; Iterate over all of the empty positions in the terminal.
           #:unless (regions:at-pos x0 y0)
           ;; Within each row of our search from here, we track the furthest
           ;; column that was acceptable. We do not search past this in
           ;; subsequent rows.
           #:do [(define prev-xf #f)])
    ;; Once an empty position is found, iterate over the remaining rows that
    ;; have an empty initial position.
    (for/or ([yf (in-col y0)]
             ;; If a region is found in the row-initial position, terminate
             ;; loop.
             #:break (regions:at-pos x0 yf))
      ;; Within each viable row, scan to the right for empty positions.
      (for/or ([xf (in-row x0 prev-xf)]
               ;; If a region is encountered, terminate inner loop.
               #:break (regions:at-pos xf yf)
               ;; If an empty position is found, update our max column info.
               ;; This is used by subsequent row scans to ensure we only test
               ;; against rectangular spaces.
               #:do [(set! prev-xf xf)])
        ;; We check whether the current width and height is acceptable. If it
        ;; is, we terminate the search.
        (and (test-cols (add1 (- xf x0)))
             (test-rows (add1 (- yf y0)))
             (list x0 y0 xf yf))))))


#;(define (regions:find-empty-rectangle test-cols test-rows)
  (define best-coords #f)
  (define best-width   0)
  (define best-height  0)
  (for* ([(x0 y0) (in-term)]
         ;; Iterate over all of the empty positions in the terminal.
         #:unless (regions:at-pos x0 y0)
         ;; Once an empty position is found, iterate over the remaining rows
         ;; that have an empty initial position.
         [yf (in-col y0)]
         ;; If a region is found in the initial position, terminate loop.
         #:break (regions:at-pos x0 yf)
         #:do [(define prev-xf #f)])
    (displayln (format "row: ~v" yf))
    ;; Within each viable row, scan to the right for empty positions.
    ;;
    ;; NOTE: This loop can't be subsumed by the outer loop because we want to
    ;; [#:break] whenever a region is found within a row, but continue searching
    ;; additional candidate rows afterwards.
    (for ([xf (in-row x0 prev-xf)]
          ;; If a region is encountered, terminate inner loop.
          #:break (regions:at-pos xf yf)
          ;; If an empty position is found, update our max column info. This
          ;; is used by subsequent row scans to ensure we only test against
          ;; rectangular spaces.
          #:do [(set! prev-xf xf)])
      ;; If there is not a region, we check whether our new width and height is
      ;; at least as good as the best found so far. If it is, we update what
      ;; we've found.
      (let ([width  (add1 (- xf x0))]
            [height (add1 (- yf y0))])
        (when (and (test-cols width)
                   (test-rows height)
                   (>= width best-width)
                   (>= height best-height))
          (set! best-width width)
          (set! best-height height)
          (set! best-coords (list x0 y0 xf yf))))))
  ;; Return the best coordinates we found.
  best-coords)

;; Searches for an empty rectangle of the specified size, starting from the
;; indicated start position. Returns [#f] if no such rectangle exists.
#;(define (find-empty-rectangle [width-spec 0] [height-spec 0]
                              [start-x    1] [start-y     1])
  (term:set-current-pos! 1 1)
  (let-values ([(screen-cols screen-rows) (term:screen-size)])
    (let loop ([x start-x]
               [y start-y])
      (match (regions:at-pos x y)
        ;; No region exists at the indicated position, so we want to find out
        ;; whether a region of the given specification can fit starting here.
        ;;
        ;; TODO: I wonder whether it would make sense to create a map from
        ;; positions to regions, since that kind of operation seems somewhat
        ;; common. However, there shouldn't be too many regions at any given
        ;; time, so the speedup may be rather small and not worth the effort.
        [#f
         ;; TODO: First, check whether it's even possible to fit the spec within
         ;; the screen from this point.
         ;; Search each of the needed rows from left to right to verify that
         ;; none of them contain any part of another region.
         (let ([final-x (sub1 (+ x width-spec))]
               [final-y (sub1 (+ y height-spec))])
           (let y-loop ([scan-y y])
             (let x-loop ([scan-x x])
               (match (regions:at-pos scan-x scan-y)
                 ;; No obstacle; continue to next viable address or finish if
                 ;; we're done.
                 [#f
                  (cond
                    ;; Complete success. Return result.
                    [(and (= scan-x final-x)
                          (= scan-y final-y))
                     (list x y final-x final-y)]
                    ;; Done with this row but we're not at the end of the spec.
                    ;; Move to the next row, if possible.
                    [(= scan-x final-x)
                     (if (< scan-y screen-rows)
                         ;; More rows, so we move on!
                         (y-loop (add1 scan-y))
                         ;; No more rows; failure.
                         #f)]
                    ;; Move to the next column, if possible.
                    [else
                     (if (< scan-x screen-cols)
                         ;; More columns, so we move on!
                         (x-loop (add1 scan-x))
                         ;; No more columns; failure.
                         #f)])]
                 ;; We've found an obstacle, so we want to skip over it. To do
                 ;; this, we try to skip to the next available column. If that's
                 ;; not possible, we'll skip to the next available row.
                 [(list _ _ obstacle-max-x _)
                  (if (< screen-cols (+ obstacle-max-x width-spec))
                      (loop 1 (add1 y))
                      (loop (add1 obstacle-max-x) y))]))))]
        ;; A region was found at the indicated position. We do some thinking to
        ;; skip ahead to the next position that might be empty.
        [(list _ min-x min-y max-x max-y)
         (cond
           ;; Region fills whole last row. No lines left.
           ;;
           ;; |X1111|
           ;; +-----+
           [(and (= max-x screen-cols)
                 (= min-x 1)
                 (= max-y screen-rows))
            #f]
           ;; Region fills whole row. Skip to next line after region.
           ;;
           ;; |X1111|
           ;; |@    |
           [(and (= max-x screen-cols)
                 (= min-x 1))
            (loop 1 (add1 max-y))]
           ;; Region fills to end of this row, but a previous region may be
           ;; shorter, leaving space. Start on next row.
           ;;
           ;; |11X22|
           ;; |@ 222|
           [(and (= max-x screen-cols)
                 (= max-y screen-rows))
            (loop 1 (add1 min-y))]
           ;; It's just a region that we know doesn't extend to the end of this
           ;; row. Move to the next column on the current row after the region.
           ;;
           ;; |X11@ |
           [else
            (loop (add1 max-x) y)])]))))

(define (regions:at-pos x y)
  (let loop ([regions (current-regions)])
    (match regions
      ['() #f]
      [(cons region regions)
       (match (region-coords region)
         [(and coords
               (list from-x from-y to-x to-y))
          (or (and (<= from-x x to-x)
                   (<= from-y y to-y)
                   (cons region coords))
              (loop regions))])])))
