#lang racket

(provide make-frame
         frame-current-window
         frame-select-window!
         frame-initialize!
         frame-refresh!
         frame-add-window!
         frame-add-window-and-select!)

(require "rect.rkt")

;; Borrowing from emacs terminology a bit, a "frame" is a whole TUI interface
;; that is composed of "windows". Each window has an "application" that governs
;; how that window behaves. Applications are kind of like a merger of emacs
;; modes and buffers: an app has its own state and mode of operation.
(struct window (rect app) #:transparent)
(struct frame (space windows [current-window-id #:mutable]) #:transparent)

(define (make-frame term-size)
  (frame (make-rect term-size) (make-hash) #f))

(define frame-current-window
  (match-lambda
    [(frame _ ws curr-id) (hash-ref ws curr-id)]))

(define (frame-window-id? f w-id)
  (hash-has-key? (frame-windows f) w-id))

(define (frame-select-window! f w-id)
  (unless (frame-window-id? w-id)
    (raise-argument-error 'frame-select-window! "frame-window-id?" w-id))
  (set-frame-current-window-id! f w-id)
  ;; FIXME
  (error 'frame-select-window! "functionality unimplemented"))

(define (frame-initialize! f)
  ;; FIXME
  (error 'frame-initialize! "functionality unimplemented"))

(define (frame-refresh! f)
  ;; FIXME
  (error 'frame-refresh! "functionality unimplemented"))

(define (frame-window f w-id [failure-result
                              (thunk (raise-arguments-error
                                      'frame-window
                                      "no window found in frame with name"
                                      "name" w-id))])
  (hash-ref (frame-windows f) w-id failure-result))

(define (frame-add-window! f w-id w-app w-size where-specs
                           [who 'frame-add-window!])
  (when (frame-window-id? w-id)
    (raise-arguments-error who
                           "window name already exists in frame"
                           "name" w-id
                           "frame" f))
  (unless (size<=? w-size (frame-space f))
    (raise-arguments-error who
                           "frame too small for window"
                           "frame size" (frame-space f)
                           "window size" w-size))
  (unless (and (list? where-specs)
               (andmap symbol? (flatten where-specs)))
    (raise-argument-error who
                          "(listof symbol?)"
                          where-specs))

  (define target-selector
    (hash 'frame  (λ () (frame-space f))
          'origin (λ () unit-o)))

  (define (special-target? x)
    (hash-has-key? target-selector x))

  (define (target? x)
    (or (special-target? x)
        (frame-window-id? f x)))

  (let* ([w-rect (for/fold ([this-r (make-rect w-size)])
                           ([where-spec (in-list where-specs)])
                   (match where-spec
                     [(list 'set (? direction? this-d) (list (? direction? that-d) (? target? that)))
                      (let* ([this-base-p (rect-posn this-r this-d)]
                             [that-r (if (special-target? that)
                                         ((hash-ref target-selector that))
                                         (frame-window f that))]
                             [that-base-p (if (posn? that-r)
                                              that-r
                                              (rect-posn that-r that-d))]
                             [scalar (posn- that-base-p this-base-p)])
                        (rect-shift this-r scalar))]
                     [_ (raise-argument-error who
                                              "where-specification?"
                                              where-spec)]))]
         [w (window w-rect w-app)])
    (hash-set! (frame-windows f) w-id w)
    (unless (frame-current-window-id f)
      (frame-select-window! f w-id))))

(define (frame-add-window-and-select! f w-id w-app w-size where-specs)
  (frame-add-window! f w-id w-app w-size where-specs
                     'frame-add-window-and-select!)
  (frame-select-window! f w-id))
