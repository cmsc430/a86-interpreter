#lang racket

(provide in-term
         in-col
         in-col/reversed
         in-row
         in-row/reversed
         in-region
         for/pos
         for/pos/screen
         for/pos/col
         for/pos/row)

(require "state.rkt"
         (submod "state.rkt" private)

         (for-syntax syntax/parse))

(define (in-term [x 1] [y 1])
  (make-do-sequence
   (thunk
    (values
     ;; pos -> values
     (match-lambda [(pos x y) (values x y)])
     ;; pos -> next pos
     (match-lambda
       [(pos x y)
        (if (= x screen-cols)
            (pos 1 (add1 y))
            (pos (add1 x) y))])
     ;; initial pos
     (pos x y)
     ;; continue from pos?
     (match-lambda [(pos x y) (<= y screen-rows)])
     ;; unneeded
     #f #f))))

(define (%pos-in-seq ortho next-pos-func lo hi reversed? max-val)
  (make-do-sequence
   (thunk
    (values
     ;; current pos -> pos
     identity
     ;; current pos -> next pos
     next-pos-func
     ;; initial pos
     ;; continue from pos?
     ;; unneeded
     #f #f))))

(define (%in-seq lo hi reversed? max-val)
  (make-do-sequence
   (thunk
    (values
     ;; current v -> sequence v
     identity
     ;; current v -> next v
     (λ (v) ((if reversed? sub1 add1) v))
     ;; initial v
     (if reversed? hi lo)
     ;; continue from v?
     (λ (v) (if reversed?
                (>= v lo)
                (<= v hi)))
     ;; unneeded
     #f #f))))

(define (check-pos-range-generic who type a b step)
  (unless (onscreen-row? a) (raise-argument-error who type a))
  (unless (onscreen-row? b) (raise-argument-error who type b))
  (unless (integer? step) (raise-argument-error who "integer?" step)))

(define (in-col-generic who x a b step)
  (check-pos-range-generic who "onscreen-row?" a b step)
  (sequence-map (λ (y) (pos x y)) (in-inclusive-range a b step)))

(define in-col
  (case-lambda
    [()           (in-col current-col 1 screen-rows 1)]
    [(x)          (in-col x 1 screen-rows 1)]
    [(x a)        (in-col x a screen-rows 1)]
    [(x a b)      (in-col x a b 1)]
    [(x a b step) (in-col-generic 'in-col x a b step)]))

(define in-col/reversed
  (case-lambda
    [()           (in-col/reversed current-col 1 screen-rows 1)]
    [(x)          (in-col/reversed x 1 screen-rows 1)]
    [(x a)        (in-col/reversed x a screen-rows 1)]
    [(x a b)      (in-col/reversed x a b 1)]
    [(x a b step) (in-col-generic 'in-col/reversed x b a (* -1 step))]))

(define (in-row-generic who y a b step)
  (check-pos-range-generic who "onscreen-col?" a b step)
  (sequence-map (λ (x) (pos x y)) (in-inclusive-range a b step)))

(define in-row
  (case-lambda
    [()           (in-row current-row 1 screen-cols 1)]
    [(y)          (in-row y 1 screen-cols 1)]
    [(y a)        (in-row y a screen-cols 1)]
    [(y a b)      (in-row y a b 1)]
    [(y a b step) (in-row-generic 'in-row y a b step)]))

(define in-row/reversed
  (case-lambda
    [()           (in-row/reversed current-row 1 screen-cols 1)]
    [(y)          (in-row/reversed y 1 screen-cols 1)]
    [(y a)        (in-row/reversed y a screen-cols 1)]
    [(y a b)      (in-row/reversed y a b 1)]
    [(y a b step) (in-row-generic 'in-row/reversed y b a (* -1 step))]))

(define (in-coords from-x from-y to-x to-y
                   #:skip-interior? [skip-interior? #f]
                   #:skip-border?   [skip-border?   #f])
  ;; NO RESTRICTIONS
  ;;
  ;; The next position is either one column to the right, or the first column in
  ;; the next row.
  (define pos->next-pos
    (match-lambda
      [(pos x y)
       (if (= x to-x)
           (pos  from-x (add1 y))
           (pos (add1 x)      y))]))
  ;; SKIPPING THE INTERIOR
  ;;
  ;; If we're in the first or last row, we address every column. If we're in the
  ;; last column, we move to the first column of the next row. Otherwise, we
  ;; must be in the first column of an interior row, so we skip to the last
  ;; column of the same row.
  (define pos->next-pos/skip-interior
    (match-lambda
      [(pos x y)
       (cond
         [(= x to-x)                   (pos  from-x (add1 y))]
         [(or (= y from-y) (= y to-y)) (pos (add1 x)      y)]
         [else                         (pos    to-x       y)])]))
  ;; SKIPPING THE BORDER
  ;;
  ;; Skipping the first and last row are handled for us externally, so we need
  ;; only worry about skipping the first and last column within each row. This
  ;; looks similar to the NO RESTRICTIONS version (above), but offset by 1: the
  ;; next position is either one column to the right, or the second column in
  ;; the next row.
  (define pos->next-pos/skip-border
    (match-lambda
      [(pos x y)
       (if (= x (sub1 to-x))
           (pos (add1 from-x) (add1 y))
           (pos (add1      x)       y))]))

  (if (and skip-interior? skip-border?)
      ;; If we're skipping everything, just return an empty list.
      (list)
      ;; Otherwise, make a sequence that generates the needed coordinates.
      (make-do-sequence
       (thunk
        (values
         ;; pos -> values
         (match-lambda [(pos x y) (values x y)])
         ;; pos -> next pos
         (cond
           [skip-interior? pos->next-pos/skip-interior]
           [skip-border?   pos->next-pos/skip-border]
           [else           pos->next-pos])
         ;; initial pos
         (if skip-border?
             (pos (add1 from-x) (add1 from-y))
             (pos       from-x        from-y))
         ;; continue from pos?
         (if skip-border?
             (match-lambda [(pos _ y) (<  y to-y)])
             (match-lambda [(pos _ y) (<= y to-y)]))
         ;; unneeded
         #f #f)))))

(define (in-region from-pos to-pos
                   #:skip-interior? [skip-interior? #f]
                   #:skip-border?   [skip-border?   #f])
  (unless (pos? from-pos) (raise-argument-error 'in-region "pos?" from-pos))
  (unless (pos? to-pos)   (raise-argument-error 'in-region "pos?" to-pos))
  (define (pos->next-pos p)
    (if (pos-col=? p to-pos)
        (pos-with-col (next-row p) from-pos)
        (next-col p)))
  (define (pos->next-pos/skip-interior p)
    (cond
      [(pos-col=? p to-pos)       (pos-with-col (next-row p) from-pos)]
      [(or (pos-row=? p from-pos)
           (pos-row=? p to-pos))  (next-col p)]
      [else                       (pos-with-col p to-pos)]))
  (define (pos->next-pos/skip-border p)
    (if (pos-col=? p (prev-col to-pos))
        (pos-with-col (next-row p) from-pos)
        (next-col p)))
  (if (and skip-interior? skip-border?)
      (list)
      (make-do-sequence
       (thunk
        (values
         ;; pos -> pos
         identity
         ;; pos -> next pos
         (cond
           [skip-interior? pos->next-pos/skip-interior]
           [skip-border?   pos->next-pos/skip-border]
           [else           pos->next-pos])
         ;; initial pos
         (if skip-border?
             (next-row (next-col from-pos))
             from-pos)
         ;; continue with pos?
         (if skip-border?
             (λ (p) (pos-row<?  p to-pos))
             (λ (p) (pos-row<=? p to-pos)))
         ;; unneeded
         #f #f)))))

#;(define-syntax (for/term stx)
  (syntax-parse stx
    [(_ (p:id pos-gen-expr)
        (for-clause ...)
        body-or-break ... body)
     #'(with-saved-pos
         (for ([p pos-gen-expr]
               #:do [(unless (pos? p)
                       (raise-argument-error 'for/term "pos?" p))]
               for-clause ...)
           (unsafe-set-current-pos! p)
           body-or-break ... body))]))

(define-syntax-rule (for/pos (id pos-gen-expr)
                      (for-clause ...)
                      body-or-break ... body)
  (with-saved-pos
    (for ([id pos-gen-expr]
          #:do [(unless (pos? id)
                  (raise-argument-error 'for-term "pos?" id))]
          for-clause ...)
      (unsafe-set-current-pos! id)
      body-or-break ... body)))

(define-syntaxes (for/pos/screen for/pos/col for/pos/row)
  (let ()
    (define make-for/pos
      (case-lambda
        [(pos-expr)
         (λ (stx)
           (syntax-case stx ()
             [(_ (for-clause ...) body-or-break ... body)
              #`(for/pos (p #,pos-expr)
                  (for-clause ...)
                  body-or-break ... body)]))]
        [(pos-expr pos-arg-expr-proc)
         (λ (stx)
           (syntax-case stx ()
             [(_ arg (for-clause ...) body-or-break ... body)
              #`(for/pos (p (#,pos-arg-expr-proc arg))
                  (for-clause ...)
                  body-or-break ... body)]
             [(_ (for-clause ...) body-or-break ... body)
              #`(for/pos (p #,pos-expr)
                  (for-clause ...)
                  body-or-break ... body)]))]))
    (values (make-for/pos #'(in-region screen-first-pos screen-last-pos))
            (make-for/pos #'(in-col)
                          #'in-col)
            (make-for/pos #'(in-row)
                          #'in-row))))


;; for/term
;; for/term/screen
;; for/term/region
;; for/term/col
;; for/term/row
