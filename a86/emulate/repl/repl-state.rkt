#lang racket

(provide make-repl-state

         current-repl-state
         current-repl-show-proc
         current-repl-runtime
         current-repl-input-port
         current-repl-output-port
         current-repl-input-port->string
         current-repl-output-port->string)

(module+ private
  (provide (struct-out repl-state)))

(struct repl-state
  ([show-proc   #:mutable]
   [runtime     #:mutable]
   [input-port  #:mutable]
   [output-port #:mutable]))

(define (make-repl-state show-proc runtime input-port output-port)
  (repl-state show-proc runtime input-port output-port))

(define current-repl-state (make-parameter #f))

(define (current-repl-show-proc)
  (repl-state-show-proc (current-repl-state)))

(define (current-repl-runtime)
  (repl-state-runtime (current-repl-state)))

(define (current-repl-input-port)
  (repl-state-input-port (current-repl-state)))

(define (current-repl-output-port)
  (repl-state-output-port (current-repl-state)))

(define (current-repl-input-port->string #:from-beginning? [from-beginning? #f])
  (let ([in (current-repl-input-port)])
    (and in
         (input-port? in)
         (let ([fp (file-position* in)]
               [_  (when from-beginning?
                     (file-position in 0))]
               [rs (port->string in #:close? #f)])
           (file-position in fp)
           rs))))

(define (current-repl-output-port->string)
  (let ([out (current-repl-output-port)])
    (and out
         (get-output-string out))))
