#lang racket

(provide set-color!
         reset-color!
         with-color
         term-display)

(require "color-spec.rkt"
         "state.rkt"
         charterm

         (for-syntax syntax/parse))

(define (set-color! color)
  (current-color-spec (make-color-spec color))
  (charterm-display (color-spec->escape-code (current-color-spec))))

(define (reset-color!)
  (set-color! (make-color-spec 'reset)))

(define-syntax (with-color stx)
  (syntax-parse stx
    [(_ (~or color:expr (color:expr)) body ...+)
     #'(begin0 (parameterize ([current-color-spec (make-color-spec color)])
                 (charterm-display (color-spec->escape-code (current-color-spec)))
                 body ...)
         (charterm-display (color-spec->escape-code (current-color-spec))))]
    [(_ (color ...+) body ...+)
     #'(with-color (list color ...) body ...)]))

(define (term-display #:color            [color         #f]
                      #:width            [width         #f]
                      #:max-width        [limit         (or width screen-cols)]
                      #:limit-marker     [limit-marker  ""]
                      #:limit-prefix?    [limit-prefix? #f]
                      #:min-width        [pad-to        (or width 0)]
                      #:align            [align         'left]
                      #:pad-string       [padding       " "]
                      #:right-pad-string [right-padding padding]
                      #:left-pad-string  [left-padding  padding]
                      #:separator        [sep           ""]
                      . vs)
  (with-color (or color (current-color-spec))
    (charterm-display
     (apply ~a
            vs
            #:width            width
            #:max-width        limit
            #:limit-marker     limit-marker
            #:limit-prefix?    limit-prefix?
            #:min-width        pad-to
            #:align            align
            #:pad-string       padding
            #:right-pad-string right-padding
            #:left-pad-string  left-padding
            #:separator        sep))))
