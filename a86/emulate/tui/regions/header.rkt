#lang racket

(require "../draw.rkt"
         "../region.rkt"
         "../state.rkt")

(define-region header
  #:width-spec total
  #:height-spec 5

  ;; TODO: This is apparently necessary, for some reason. It seems like the
  ;; context doesn't work unless the [define-region] macro expands to itself.
  (define-fields
    [info-row state-row left-col])

  (define-post-init ()
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       (set! info-row (add1 from-y))
       (set! state-row (+ 2 info-row))
       (set! left-col (add1 from-x))]))

  (define-method (write-info info)
    (term:set-current-pos! left-col info-row)
    (term:display #:width (sub1 this-region:width) info)
    (term:set-current-pos! (sub1 left-col) info-row))

  (define-method (write-state!)
    (term:set-current-pos! left-col state-row)
    (term:display #:width (sub1 this-region:width) (~v (current-emulator-state)))
    (term:set-current-pos! (sub1 left-col) info-row))

  #;(define-method (write-key!)
    (set-pos!)
    (term:display #:width (sub1 this-region:width) (term:last-key)))

  (define-method (redraw!)
    (term:with-saved-pos
     (match this-region:coords
       [(list from-x from-y to-x to-y)
        (draw:fill-box from-x from-y to-x to-y)]))))
