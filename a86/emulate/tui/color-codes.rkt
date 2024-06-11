#lang racket

(provide modes
         mode-names
         mode?

         colors
         color-names
         color?

         foreground-base
         background-base)

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
(define background-base 40)
