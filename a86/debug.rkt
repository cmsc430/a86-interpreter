#lang racket

(provide debug-on?
         debug-on!
         debug-off!
         begin/debug
         define/debug
         debug)

(require (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket/string))

;; Whether debugging is currently active. This is provided to allow implementing
;; additional debugging functionality in other modules, but it is recommended to
;; only modify this flag via [debug-on!] and [debug-off!].
(define debug-on? (make-parameter #f))

;; This indentation level is automatically prefixed to any message output via
;; [debug]. The [debug] macros will automatically increase indentation by the
;; amount specified in [debug-indentation-amount].
(define debug-current-depth (make-parameter 0))
(define debug-indentation-amount 2)
;; TODO: I think the depth could actually be moved forward a phase, such that
;; debugging could be depth-limited syntactically. This would allow for emitting
;; different expressions when debugging is off, to save on performance. (This
;; probably isn't a big deal, so it's low-priority, but it would be nice and
;; would also make sense.)

;; Enables debugging.
(define (debug-on!) (debug-on? #t))
;; Disables debugging.
(define (debug-off!) (debug-on? #f))

(begin-for-syntax
  (define-syntax-class debug-fmt-spec
    #:attributes (s [args 1])
    (pattern s:str #:with (args ...) #'())
    (pattern (s:str args:expr ...))))

(define-syntax (begin/debug stx)
  (syntax-parse stx
    [(_ dfs:debug-fmt-spec e:expr ...)
     #'(begin
         (debug dfs.s dfs.args ...)
         (parameterize ([debug-current-depth (add1 (debug-current-depth))])
           e ...))]))

;; TODO: Allow specifying a list of args to print.
(define-syntax (define/debug stx)
  (syntax-parse stx
    [(_ [header:function-header
         dfs:debug-fmt-spec]
        body ...+)
     #'(define header
         (begin/debug
           dfs
           body ...))]
    [(_ header:function-header body ...+)
     (let* ([arg-strs (map (λ (arg-stx) (format "[~a : ~~a]" (syntax-e arg-stx)))
                           (syntax-e #'header.params))]
            [fmt-str (string-join arg-strs " "
                                  #:before-first (format "~a: " (syntax-e #'header.name)))])
       #`(define/debug [header
                        (#,fmt-str #,@(syntax-e #'header.params))]
           body ...))]))

;; Prints the given string with the formatted arguments, but only if debugging
;; is currently active.
(define-syntax (debug stx)
  (syntax-parse stx
    [(_ s:str args:expr ...)
     #'(when (debug-on?)
         (display (make-string (* debug-indentation-amount (debug-current-depth)) #\space))
         (displayln (format s args ...)))]))
