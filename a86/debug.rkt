#lang racket

(provide debug-on?
         debug-on!
         debug-off!
         with-debug
         debug)

(require (for-syntax syntax/parse))

;; Whether debugging is currently active. This is provided to allow implementing
;; additional debugging functionality in other modules, but it is recommended to
;; only modify this flag via [debug-on!] and [debug-off!].
(define debug-on? (make-parameter #f))

;; Enables debugging.
(define (debug-on!) (debug-on? #t))
;; Disables debugging.
(define (debug-off!) (debug-on? #f))

;; Enables debugging within the body.
(define-syntax (with-debug stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(parameterize ([debug-on? #t])
         body ...)]))

;; Prints the given string with the formatted arguments, but only if debugging
;; is currently active.
(define-syntax (debug stx)
  (syntax-parse stx
    [(_ s)
     #'(when (debug-on?)
         (displayln s))]
    [(_ s args ...+)
     #'(when (debug-on?)
         (displayln (format s args ...)))]))
