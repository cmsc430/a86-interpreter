#lang racket

(provide current-posn
         current-col
         current-row
         onscreen-col?
         onscreen-row?
         onscreen-posn?
         set-current-posn!

         screen-size
         screen-cols
         screen-rows
         screen-first-posn
         screen-last-posn

         last-key

         with-posn
         with-saved-posn
         with-term-loop)

(require "rect.rkt"
         "frame.rkt"
         "frameset.rkt"
         charterm

         (for-syntax syntax/parse))

(define screen-size-param
  (make-parameter #f #f 'screen-size))

(define current-posn-param
  (make-parameter #f #f 'current-posn))

(define last-key-param
  (make-parameter #f #f 'last-key))

(define current-frameset-param
  (make-parameter #f #f 'current-frameset))

(define-syntax (define-property-ids stx)
  (syntax-parse stx
    [(_ (name:id expr) ...+)
     #'(begin
         (define-syntax (name stx)
           (syntax-case stx () [val (identifier? #'val) #'expr])) ...)]))

(define-property-ids
  [screen-size       (screen-size-param)]
  [screen-cols       (size-w screen-size)]
  [screen-rows       (size-h screen-size)]
  [screen-first-posn (posn 0 0)]        ;; TODO: what is this for?
  [screen-last-posn  (posn screen-cols screen-rows)]

  [current-posn      (current-posn-param)]
  [current-col       (posn-x current-posn)]
  [current-row       (posn-y current-posn)]

  [last-key          (last-key-param)]

  [current-frameset  (current-frameset-param)]
  [current-frame     (frameset-current-frame current-frameset)]
  [current-window    (frame-current-window current-frame)])

(define (update-cursor!)
  ;; NOTE: We use 0-indexed positions, but charterm is implemented using
  ;; 1-indexed cursor positions. The translation between these happens here and
  ;; nowhere else.
  (charterm-cursor (add1 current-col) (add1 current-row)))

(define (onscreen-col? x) (and (exact-nonnegative-integer? x)
                               (< x screen-cols)))
(define (onscreen-row? y) (and (exact-nonnegative-integer? y)
                               (< y screen-rows)))

(define onscreen-posn?
  (match-lambda
    [(posn x y) (and (onscreen-col? x) (onscreen-row? y))]
    [v (raise-argument-error 'onscreen-posn? "posn?" v)]))

(define (assert-onscreen-posn! p)
  (unless (onscreen-posn? p)
    (error 'assert-onscreen-posn! "posn not onscreen: ~v" p)))

(define (unsafe-set-current-posn! p [who 'unsafe-set-current-posn!])
  (match p
    [(and (posn x y) p)
     (current-posn-param p)
     (update-cursor!)]
    [v (raise-argument-error who v)]))

(define (set-current-posn! p)
  (assert-onscreen-posn! p)
  (unsafe-set-current-posn! p 'set-current-posn!))

(define-syntax-rule (with-posn (posn-expr) body1 body ...)
  (let ([p posn-expr])
    (assert-onscreen-posn! p)
    (begin0 (parameterize ([current-posn-param p])
              (update-cursor!)
              body1 body ...)
      (update-cursor!))))

(define-syntax-rule (with-saved-posn body1 body ...)
  (with-posn (current-posn) body1 body ...))

(define-syntax (with-term-loop stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:init [init-body ...+])
                   #:defaults ([(init-body 1) '()]))
        break-id:id
        ([parameter-id:id parameter-expr] ...)
        loop-id:id
        ([loop-param-id:id loop-param] ...)
        loop-body ...+)
     #'(with-charterm
         ;; Set the initial screen size.
         ;;
         ;; TODO: Currently, this can't be updated anywhere. Eventually it would
         ;; be good to support a sort of refreshing operation that will check if
         ;; the screen size has changed and update the parameter appropriately.
         (screen-size-param (call-with-values (thunk (charterm-screen-size))
                                              size))
         ;; Clear the screen and set the initial posnition to the top-left.
         (charterm-clear-screen)
         (set-current-posn! unit-o)
         (update-cursor!)
         ;; The client can add some additional initialization functionality.
         init-body ...
         ;; Initialize an escape continuation for terminating execution.
         (let/ec break-id
           ;; Various factors may need to be parameterized. Support those.
           (parameterize ([parameter-id parameter-expr] ...)
             ;; Set up the terminal's looping process with the optional
             ;; parameters.
             (let loop-id ([loop-param-id loop-param] ...)
               loop-body ...))))]))
