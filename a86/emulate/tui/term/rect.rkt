#lang racket

(provide (struct-out rect)

         make-rect

         rect-posns
         rect-posn
         rect-size
         rect-area
         rect-width
         rect-height
         rect-dimensions

         rect-shift

         split-rect-horizontal
         split-rect-vertical
         split-rect-in-half-horizontal
         split-rect-in-half-vertical
         split-rect-in-fourths

         rect-contains?
         rect-intersect
         rect-interior

         in-rect
         in-interior
         in-border

         in-north-edge
         in-east-edge
         in-south-edge
         in-west-edge

         in-north-edge?
         in-east-edge?
         in-south-edge?
         in-west-edge?
         in-border?

         (all-from-out "posn.rkt"
                       "size.rkt"))

(require "posn.rkt"
         "size.rkt"

         (for-syntax racket/syntax
                     syntax/parse))

(struct rect (nw ne sw se) #:transparent)

(define make-rect
  (match-lambda*
    [(list (size w h))
     (make-rect unit-o (posn (sub1 w) (sub1 h)))]
    [(list (? posn? nw) (? posn? se))
     (let ([ne (posn-with-col nw se)]
           [sw (posn-with-col se nw)])
       (make-rect nw ne sw se))]
    [(list (? posn? nw) (? posn? ne) (? posn? sw) (? posn? se))
     (unless (and (posn<? nw ne)
                  (posn<? nw sw)
                  (posn<? sw se)
                  (posn<? nw se))
       (raise-arguments-error 'make-rect
                              "invalid corners specified"
                              "northwest" nw
                              "northeast" ne
                              "southwest" sw
                              "southeast" se))]))

(define rect-posns
  (match-lambda
    [(rect nw ne sw se) (values nw ne sw se)]))

(define (rect-posn r d)
  (let-values ([(r-nw r-ne r-sw r-se) (rect-posns r)])
    (case d
      [(nw n w) r-nw]
      [(ne e)   r-ne]
      [(sw s)   r-sw]
      [(se)     r-se]
      [else (raise-argument-error 'rect-posn "direction?" d)])))

(define rect-size
  (match-lambda
    [(rect nw _ _ se)
     (posn-bind size (posn- se nw (posn -1 -1)))]))

(define (rect-area r)
  (match (rect-size r)
    [(size w h)
     (* w h)]))

(define (rect-width r)
  (size-w (rect-size r)))

(define (rect-height r)
  (size-h (rect-size r)))

(define (rect-dimensions r)
  (match (rect-size r)
    [(size w h) (values w h)]))

(define rect-shift
  (match-lambda*
    [(list (rect nw ne sw se) (? posn? scalar))
     (rect (posn+ nw scalar)
           (posn+ ne scalar)
           (posn+ sw scalar)
           (posn+ se scalar))]
    [(list (? rect? r) (? direction? d) (? exact-integer? n))
     (let* ([unit (get-unit d)]
            [scalar (posn* unit (posn n n))])
       (rect-shift r scalar))]))

(define (split-rect who r first-size dimension)
  (let-values ([(posn-with-maj-dim posn-with-min-dim add-maj-dim next-maj-dim rect-dim)
                (match dimension
                  ['horizontal (values posn-with-row posn-with-col add-rows next-row rect-height)]
                  ['vertical   (values posn-with-col posn-with-row add-cols next-col rect-width)]
                  [_ (raise-argument-error who "dimension in '(horizontal vertical)" dimension)])])
    (match r
      [(rect nw0 _ _ se0)
       (let* ([s0 (rect-dim r)]
              [s1 (match first-size
                    ['half (sub1 (ceiling (/ s0 2)))]
                    [(? exact-nonnegative-integer?)
                     (unless (<= first-size s0)
                       (raise-argument-error
                        who
                        (format "first-size contained in ~a dimension (~a)" dimension s0)
                        first-size))
                     (sub1 first-size)]
                    [_
                     (raise-argument-error
                      who
                      (format "either 'half or exact-nonnegative-integer? contained in ~a dimension (~a)"
                              dimension s0)
                      first-size)])]
              [nw1 nw0]
              [se1 (posn-with-maj-dim se0 (add-maj-dim nw0 s1))]
              [nw2 (next-maj-dim (posn-with-min-dim se1 nw0))]
              [se2 se0])
         (values (make-rect nw1 se1)
                 (make-rect nw2 se2)))])))

(define (split-rect-horizontal r first-size) (split-rect 'split-rect-horizontal r first-size 'horizontal))
(define (split-rect-vertical   r first-size) (split-rect 'split-rect-vertical   r first-size 'vertical))

(define (split-rect-in-half-horizontal r) (split-rect 'split-rect-in-half-horizontal r 'half 'horizontal))
(define (split-rect-in-half-vertical   r) (split-rect 'split-rect-in-half-vertical   r 'half 'vertical))

(define (split-rect-in-fourths r)
  (let*-values ([(rn  rs)  (split-rect-in-half-horizontal r)]
                [(rnw rne) (split-rect-in-half-vertical rn)]
                [(rsw rse) (split-rect-in-half-vertical rs)])
    (values rnw rne rsw rse)))

(define (posn-in-rect? r p)
  (match r
    [(rect nw _ _ se)
     (posn<=? nw p se)]))

(define (posns-in-rect? r . ps)
  (andmap (λ (p) (posn-in-rect? r p)) ps))

(define (rect-contains? r1 r2)
  (posns-in-rect? r1 (rect-posns r2)))

(define (rect-intersect r1 r2)
  (define one-way-intersect
    (match-lambda**
     [((and r1 (rect nw1 ne1 sw1 se1)) (and r2 (rect nw2 ne2 sw2 se2)))
      (match/values (apply values (for/list ([p (rect-posns r2)]) (posn-in-rect? r1 p)))
        ;; ┌──────┐
        ;; │ ┌──┐ │
        ;; │ └──┘ │
        ;; └──────┘
        [(#t #t #t #t) r2]
        ;; ┌──────┐
        ;; │ ┌──┐ │
        ;; └─│  │─┘
        ;;   └──┘
        [(#t #t #f #f) (rect nw2 ne2 (posn-with-row sw2 sw1) (posn-with-row se2 se1))]
        ;;   ┌───┐
        ;; ┌───┐ │
        ;; └───┘ │
        ;;   └───┘
        [(#t #f #t #f) (rect (posn-with-col nw2 nw1) ne2 (posn-with-col sw2 sw1) se2)]
        ;;   ┌──┐
        ;; ┌─│  │─┐
        ;; │ └──┘ │
        ;; └──────┘
        [(#f #f #t #t) (rect (posn-with-row nw2 nw1) (posn-with-row ne2 ne1) sw2 se2)]
        ;; ┌───┐
        ;; │ ┌───┐
        ;; │ └───┘
        ;; └───┘
        [(#f #t #f #t) (rect nw2 (posn-with-col ne2 ne1) sw2 (posn-with-col se2 se1))]
        ;; ┌─────┐
        ;; │  ┌───┐
        ;; └──│   │
        ;;    └───┘
        [(#t #f #f #f) (rect nw2 (posn-with-col ne2 ne1) (posn-with-row sw2 sw1) se1)]
        ;;  ┌─────┐
        ;; ┌───┐  │
        ;; │   │──┘
        ;; └───┘
        [(#f #t #f #f) (rect (posn-with-col nw2 nw1) ne2 sw1 (posn-with-row se2 se1))]
        ;;    ┌───┐
        ;; ┌──│   │
        ;; │  └───┘
        ;; └─────┘
        [(#f #f #t #f) (rect (posn-with-row nw2 nw1) ne1 sw2 (posn-with-col se2 se1))]
        ;; ┌───┐
        ;; │   │──┐
        ;; └───┘  │
        ;;  └─────┘
        [(#f #f #f #t) (rect nw1 (posn-with-row ne2 ne1) (posn-with-col sw2 sw1) se2)]
        ;; ┌──┐
        ;; │  │ ┌────┐
        ;; └──┘ │    │
        ;;      └────┘
        [(#f #f #f #f) #f])]))
  ;; We try to find the intersection both ways.
  (or (one-way-intersect r1 r2)
      (one-way-intersect r2 r1)))

(define rect-interior
  (match-lambda
    [(rect nw0 ne0 sw0 se0)
     (let ([nw1 (go-se nw0)]
           [ne1 (go-sw ne0)]
           [sw1 (go-ne sw0)]
           [se1 (go-nw se0)])
       (rect nw1 ne1 sw1 se1))]))

(define in-rect
  (match-lambda
    [(rect nw ne sw se)
     (apply stream-append
            (for/list ([w (in-col nw sw)]
                       [e (in-col ne se)])
              (in-row w e)))]))

(define (in-interior r) (in-rect (rect-interior r)))

(define in-border
  (match-lambda
    [(rect nw ne sw se)
     (stream-append (in-row nw ne)
                    (in-col (next-row nw) (prev-row sw))
                    (in-col (next-row ne) (prev-row se))
                    (in-row sw se))]))

(define-syntax (define-edge-stream stx)
  (syntax-parse stx
    [(_ name:id row-or-col:id lo:id hi:id)
     #:fail-when (and (not (memq (syntax-e #'row-or-col) '(row col))) #'row-or-col)
     "dimension not in '(row col)"
     #:fail-when (or (and (not (memq (syntax-e #'lo) '(nw ne sw se))) #'lo)
                     (and (not (memq (syntax-e #'hi) '(nw ne sw se))) #'hi))
     "accessor not in '(nw ne sw se)"
     #:with in-dim (format-id #'row-or-col #:source #'row-or-col "in-~a" #'row-or-col)
     #:with rect-lo (format-id #'lo #:source #'lo "rect-~a" #'lo)
     #:with rect-hi (format-id #'hi #:source #'hi "rect-~a" #'hi)
     #'(define (name r #:reversed? [reversed? #f])
         (if reversed?
             (in-dim (rect-hi r) (rect-lo r) -1)
             (in-dim (rect-lo r) (rect-hi r))))]))

(define-edge-stream in-north-edge row nw ne)
(define-edge-stream in-west-edge  col nw sw)
(define-edge-stream in-south-edge row sw se)
(define-edge-stream in-east-edge  col ne se)

(define in-north-edge? (match-lambda** [((rect nw ne _ _) p)  (and (posn-row=? nw p) (posn-col<=? nw p ne))]))
(define in-west-edge?  (match-lambda** [((rect nw _ sw _) p)  (and (posn-col=? nw p) (posn-row<=? nw p sw))]))
(define in-south-edge? (match-lambda** [((rect _ _ sw se) p)  (and (posn-row=? sw p) (posn-col<=? sw p se))]))
(define in-east-edge?  (match-lambda** [((rect _ ne _ se) p)  (and (posn-col=? ne p) (posn-row<=? ne p se))]))

(define (in-border? r p)
  (or (in-north-edge? r p)
      (in-west-edge?  r p)
      (in-south-edge? r p)
      (in-east-edge?  r p)))

#;(define (rect-insert r1 r2)
  (unless (rect-contains? r1 r2)
    (raise-arguments-error 'rect-insert
                           "r2 not fully contained by r1"
                           "r1" r1
                           "r2" r2))
  (match/values (apply values (for/list ([p (rect-posns r2)]) (in-border? r1 p)))
    ;; ┌──────┐    ┌─┬────┐
    ;; │ ┌──┐ │    │ ├──┬─┤      maybe this option
    ;; │ └──┘ │    ├─┴──┤ │    should not be allowed
    ;; └──────┘    └────┴─┘
    [(#f #f #f #f) #f]
    ;; ┌──┬───┐    ┌──┬───┐    ┌──┬───┐
    ;; │  │   │    │  │   │    │  │   │
    ;; ├──┘   │    ├──┤   │    ├──┴───┤
    ;; └──────┘    └──┴───┘    └──────┘
    [(#t #f #f #f) #f]
    ;; ┌───┬──┐    ┌───┬──┐    ┌───┬──┐
    ;; │   │  │    │   │  │    │   │  │
    ;; │   └──┤    │   ├──┤    ├───┴──┤
    ;; └──────┘    └───┴──┘    └──────┘
    [(#f #t #f #f) #f]
    ;; ┌──────┐    ┌──┬───┐    ┌──────┐
    ;; ├──┐   │    ├──┤   │    ├──┬───┤
    ;; │  │   │    │  │   │    │  │   │
    ;; └──┴───┘    └──┴───┘    └──┴───┘
    [(#f #f #t #f) #f]
    ;; ┌──────┐    ┌───┬──┐    ┌──────┐
    ;; │   ┌──┤    │   ├──┤    ├───┬──┤
    ;; │   │  │    │   │  │    │   │  │
    ;; └───┴──┘    └───┴──┘    └───┴──┘
    [(#f #f #f #t) #f]
    ;; ┌──┬───┐
    ;; │  │   │       no          no
    ;; │  │   │     change      change
    ;; └──┴───┘
    ;;    OR
    ;; ┌──────┐
    ;; ├──────┤       no          no
    ;; │      │     change      change
    ;; └──────┘
    [(#t #t #t #t) #f]))
