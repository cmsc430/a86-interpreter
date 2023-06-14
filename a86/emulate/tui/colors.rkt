#lang racket

(provide mode-names
         color-names
         set-color!
         reset-color!
         with-color)

(module codes racket
  (provide (all-defined-out))

  (define modes
    (hash 'reset         0
          'bold          1
          'dim           2
          'italic        3
          'underline     4
          'blink         5
          ;; 'fast-blink 6
          'inverse       7
          'invisible     8
          'strikethrough 9))
  (define mode-names (hash-keys modes))
  (define (mode? x) (hash-has-key? modes x))

  (define colors
    (hash 'black   0
          'red     1
          'green   2
          'yellow  3
          'blue    4
          'magenta 5
          'cyan    6
          'white   7
          'default 9))
  (define color-names (hash-keys colors))
  (define (color? x) (hash-has-key? colors x))

  (define foreground-base 30)
  (define background-base 40))

(require 'codes
         "term.rkt"

         (for-syntax 'codes
                     syntax/parse))

(define ESC "\x1b[")

(define-syntax (~escape stx)
  (syntax-parse stx
    [(_ args ...+)
     #'(string-append
        ESC
        (~a args ...))]))

(define (encode-color mode fg bg)
  (~escape (string-join
            (list (hash-ref modes mode)
                  (+ foreground-base (hash-ref colors fg))
                  (+ background-base (hash-ref colors bg)))
            ";")
           "m"))

(define default-color-spec (list 'reset 'default 'default))

(define current-color
  (make-parameter
   default-color-spec
   (λ (parts)
     (match (current-color)
       [(list old-mode old-fg old-bg)
        (match parts
          [(or (? color? fg)
               (list (? color? fg)))
           (list old-mode fg old-bg)]
          [(or (? mode? mode)
               (list (? mode? mode)))
           (list mode old-fg old-bg)]
          [(list (? color? fg) (? color? bg))
           (list old-mode fg bg)]
          [(list (? mode? mode) (? color? fg))
           (list mode fg old-bg)]
          [(list (? mode? mode) (? color? fg) (? color? bg))
           (list mode fg bg)]
          [_ (error 'current-color
                    "not a color spec: ~v"
                    parts)])]))
   'current-color))

(define (set-color! . parts)
  (current-color parts)
  (term:display (apply encode-color parts)))

(define (reset-color!)
  (apply set-color! default-color-spec))

(define-syntax (with-color stx)
  (define-syntax-class mode
    (pattern x #:when (and (identifier? #'x)
                           (memq (syntax-e #'x) mode-names))))
  (define-syntax-class color
    (pattern x #:when (and (identifier? #'x)
                           (memq (syntax-e #'x) color-names))))

  (syntax-parse stx
    [(_ (mode:mode fg bg)  body ...+) #'(with-color ('mode fg bg) body ...)]
    [(_ (mode fg:color bg) body ...+) #'(with-color (mode 'fg bg) body ...)]
    [(_ (mode fg bg:color) body ...+) #'(with-color (mode fg 'bg) body ...)]
    [(_ (mode fg bg)       body ...+)
     #'(parameterize ([current-color (list mode fg bg)]) body ...)]

    [(_ (mode:mode fg)  body ...+) #'(with-color ('mode fg) body ...)]
    [(_ (mode fg:color) body ...+) #'(with-color (mode 'fg) body ...)]
    [(_ (mode fg)       body ...+)
     #'(parameterize ([current-color (list mode fg)]) body ...)]

    [(_ (mode:mode)  body ...+) #'(with-color ('mode) body ...)]
    [(_ (fg:color)   body ...+) #'(with-color ('fg)   body ...)]
    [(_ (mode-or-fg) body ...+)
     #'(parameterize ([current-color (list mode-or-fg)]) body ...)]))
