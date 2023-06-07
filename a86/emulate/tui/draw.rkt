#lang racket

(provide is-in-bounds?
         is-in-border?
         is-in-interior?
         is-top-edge?
         is-bottom-edge?
         is-left-edge?
         is-right-edge?
         is-top-left-corner?
         is-top-right-corner?
         is-bottom-left-corner?
         is-bottom-right-corner?
         in-bounds
         in-border
         in-interior
         parameterize-bounds
         draw:border
         draw:fill-box)

(require "term.rkt"
         racket/stxparam

         (for-syntax syntax/parse))

(define (box-style style)
  (define (make-style-hash h v dr dl ur ul vr vl hd hu vh)
    (hash 'h h 'v v 'dr dr 'dl dl 'ur ur 'ul ul 'vr vr 'vl vl 'hd hd 'hu hu 'vh vh))
  (case style
    [(none)
     (make-style-hash #\space #\space #\space #\space #\space #\space
                      #\space #\space #\space #\space #\space)]
    [(ascii)
     (make-style-hash #\- #\| #\+ #\+ #\+ #\+ #\+ #\+ #\+ #\+ #\+)]
    [(light)
     ;; ─ │ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼
     (make-style-hash #\u2500 #\u2502 #\u250c #\u2510 #\u2514 #\u2518
                      #\u251c #\u2524 #\u252c #\u2523 #\u253c)]))

(define (draw:in-coords from-x from-y to-x to-y
                        #:skip-interior? [skip-interior? #f]
                        #:skip-border?   [skip-border?   #f])
  (if (and skip-interior? skip-border?)
      ;; If we skip everything, just return an empty list.
      (list)
      ;; Otherwise, we have to generate the elements.
      (make-do-sequence
       (thunk
        (values
         ;; pos->element            : pos -> element
         (match-lambda
           [(cons x y)
            (values x y)])
         ;; next-pos                : pos -> pos
         (match-lambda
           [(cons x y)
            ;; TODO: Simplify.
            ;; Are we addressing the interior?
            (cond
              ;; Do we skip the interior, addressing only the border?
              [skip-interior?
               ;; Are we on either the first or last line?
               (if (or (= y from-y)
                       (= y to-y))
                   ;; We are, so address all the positions.
                   (if (= x to-x)
                       (cons  from-x  (add1 y))
                       (cons (add1 x)       y))
                   ;; We are not, so skip the interior positions.
                   (if (= x to-x)
                       (cons from-x (add1 y))
                       (cons   to-x       y)))]
              ;; Do we skip the border, addressing only the interior?
              [skip-border?
               ;; We already skipped the entire first row, and the last row will
               ;; be skipped for us. We will also never yield addresses at the
               ;; left column, so we only have to check if we're about to enter
               ;; the right column.
               (if (= x (sub1 to-x))
                   (cons (add1 from-x) (add1 y))
                   (cons x y))]
              ;; We skip nothing --- address it all!
              [else
               (if (= x to-x)
                   (cons  from-x  (add1 y))
                   (cons (add1 x)       y))])])
         ;; initial pos             : pos
         (if skip-border?
             (cons (add1 from-x) (add1 from-y))
             (cons       from-x        from-y))
         ;; continue-with-pos?      : pos -> bool
         (match-lambda
           [(cons _ y)
            (if skip-border?
                (not (= y to-y))
                (not (> y to-y)))])
         ;; continue-with-val?      : element -> bool
         #f
         ;; continue-after-pos+val? : pos -> element -> bool
         #f)))))

(define-syntax (define-bounds-parameter stx)
  (syntax-parse stx
    [(_ name:id)
     #`(define-syntax-parameter name
         (λ (stx)
           (raise-syntax-error
            #f
            #,(format "cannot use '~a' outside draw:with-bounds context" (syntax-e #'name))
            stx)))]))
(define-bounds-parameter is-in-bounds?)
(define-bounds-parameter is-in-border?)
(define-bounds-parameter is-in-interior?)
(define-bounds-parameter is-top-edge?)
(define-bounds-parameter is-bottom-edge?)
(define-bounds-parameter is-left-edge?)
(define-bounds-parameter is-right-edge?)
(define-bounds-parameter is-top-left-corner?)
(define-bounds-parameter is-top-right-corner?)
(define-bounds-parameter is-bottom-left-corner?)
(define-bounds-parameter is-bottom-right-corner?)
(define-bounds-parameter in-bounds)
(define-bounds-parameter in-border)
(define-bounds-parameter in-interior)
(define-syntax (parameterize-bounds stx)
  (syntax-parse stx
    [(_ from-x from-y to-x to-y body ...+)
     #'(syntax-parameterize
           ([is-in-bounds?
             (syntax-rules () [(_ x y) (and (<= from-x x to-x) (<= from-y y to-y))])]
            [is-in-border?
             (syntax-rules () [(_ x y) (and (or (= x from-x)   (= x to-x))
                                            (or (= y from-y)   (= y to-y)))])]
            [is-in-interior?
             (syntax-rules () [(_ x y) (and (< from-x x to-x)  (< from-y y to-y))])]
            [is-top-edge?
             (syntax-rules () [(_ x y) (and (< from-x x to-x)  (= y from-y))])]
            [is-bottom-edge?
             (syntax-rules () [(_ x y) (and (< from-x x to-x)  (= y to-y))])]
            [is-left-edge?
             (syntax-rules () [(_ x y) (and (= x from-x)       (< from-y y to-y))])]
            [is-right-edge?
             (syntax-rules () [(_ x y) (and (= x to-x)         (< from-y y to-y))])]
            [is-top-left-corner?
             (syntax-rules () [(_ x y) (and (= x from-x)       (= y from-y))])]
            [is-top-right-corner?
             (syntax-rules () [(_ x y) (and (= x to-x)         (= y from-y))])]
            [is-bottom-left-corner?
             (syntax-rules () [(_ x y) (and (= x from-x)       (= y to-y))])]
            [is-bottom-right-corner?
             (syntax-rules () [(_ x y) (and (= x to-x)         (= y to-y))])]

            [in-bounds
             (syntax-rules () [(_) (draw:in-coords from-x from-y to-x to-y)])]
            [in-border
             (syntax-rules () [(_) (draw:in-coords from-x from-y to-x to-y
                                                   #:skip-interior? #t)])]
            [in-interior
             (syntax-rules () [(_) (draw:in-coords from-x from-y to-x to-y
                                                   #:skip-border? #t)])])
         body ...)]))

(define (draw:border from-x from-y to-x to-y
                     #:style [style 'light])
  (term:with-saved-pos
   (parameterize-bounds
    from-x from-y to-x to-y
    (if (eq? style 'annotated)
        ;; The annotated border has to be handled a bit differently.
        (term:with-mode
         'inverse
         (for ([(x y) (in-border)])
           (term:set-current-pos! x y)
           (cond
             [(is-top-left-corner? x y)
              (term:display "1")]
             [(and (or (is-top-edge? x y)
                       (is-top-right-corner? x y))
                   (even? (- x from-x)))
              (term:display (~r (remainder (add1 (- x from-x)) 10)))]
             [(and (or (is-left-edge? x y)
                       (is-bottom-left-corner? x y))
                   (even? (- y from-y)))
              (term:display (~r (remainder (add1 (- y from-y)) 10)))]
             [else
              (term:display " ")])))
        ;; Regular borders can all be handled the same.
        (let ([style-hash (box-style style)])
          (for ([(x y) (in-border)])
            (let* ([s (cond
                        [(or (is-top-edge? x y)
                             (is-bottom-edge? x y))    'h]
                        [(or (is-left-edge? x y)
                             (is-right-edge? x y))     'v]
                        [(is-top-left-corner? x y)     'dr]
                        [(is-top-right-corner? x y)    'dl]
                        [(is-bottom-left-corner? x y)  'ur]
                        [(is-bottom-right-corner? x y) 'ul])]
                   [c (hash-ref style-hash s)])
              (term:set-current-pos! x y)
              (term:display c))))))))

(define (draw:fill-box from-x from-y to-x to-y [c " "])
  (term:with-saved-pos
   (parameterize-bounds
    from-x from-y to-x to-y
    (for ([(x y) (in-bounds)])
      (term:set-current-pos! x y)
      (term:display c)))))
