#lang racket

(provide parse)

(define (parse line)
  (match line
    [(? eof-object?) #f]
    [(? string?)
     (with-input-from-string line
       (λ ()
         (let loop ([args '()])
           (match (read)
             [(? eof-object?) (reverse args)]
             [v (loop (cons v args))]))))]))
