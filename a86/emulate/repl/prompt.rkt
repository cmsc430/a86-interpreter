#lang racket

(provide prompt)

;; Displays the [leader] and reads input from the user until a line separator or
;; end-of-file is read.
(define (prompt [leader "a86> "])
  (display leader)
  (read-line))
