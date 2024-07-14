#lang racket

(require "../../exn.rkt")

(provide exn:fail:contract:bad-format?
         exn:fail:contract:bad-format-expected-args)

(define-a86-exn/provide user:repl ()
  #:parent-name exn:fail:a86:user)
(define-a86-exn/provide user:repl:bad-command (name args)
  #:parent-name exn:fail:a86:user:repl)
(define-a86-exn/provide user:repl:format ()
  #:parent-name exn:fail:a86:user:repl)
(define-a86-exn/provide user:repl:format:bad-arg-count (actual expected args)
  #:parent-name exn:fail:a86:user:repl:format)

(define (exn:fail:contract:bad-format? e)
  (and (exn:fail:contract? e)
       (string-prefix? (exn-message e)
                       "format: format string requires ")))

(define (exn:fail:contract:bad-format-expected-args e)
  (read-from-string (exn-message e) 31))

(define (read-from-string s [pos 0])
  (with-input-from-string s
    (λ ()
      (file-position (current-input-port) pos)
      (read))))

#;(define (read-all-from-string s [pos 0])
  (with-input-from-string s
    (λ ()
      (file-position (current-input-port) pos)
      (let loop ([ds  '()])
        (match (read)
          [(? eof-object?) (reverse ds)]
          [d (loop (cons d ds))])))))
