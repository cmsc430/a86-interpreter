#lang racket

(provide tui-loop-break
         handle-error
         handle-key
         current-keymap
         default-keymap)

(require "exn.rkt"
         "region-state.rkt"
         "state.rkt"
         "regions/main.rkt")

(define tui-loop-break
  (make-parameter
   (thunk
    (error 'tui-loop-break "no break function was parameterized"))))

(define (exit-tui)
  ((tui-loop-break)))

(define (format-error msg)
  (~a (string-replace msg "\n" " // ")
      #:width (- header:width 2)
      #:limit-marker " [...]"))

(define (handle-error err)
  (match err
    [(exn msg _)
     (header:write-info (format-error msg))]))

(define current-keymap (make-parameter (make-hash)))

;; TODO
#;(define (keydef? x) #t)

#;(define (define-key keydef functionality [keymap (current-keymap)])
  (unless (keydef? keydef)
    (error 'define-key "not a valid keydef: ~v" keydef))
  (hash-set! keymap keydef functionality))

#;(define (delete-key keydef [keymap (current-keymap)])
  (unless (hash-has-key? keymap keydef)
    (error 'delete-key "key not bound in keymap: ~v" keydef))
  (hash-remove! keymap keydef))

(define (refresh)
  (header:write-state!)
  (instructions:refresh-state!)
  (registers:write-flags!)
  (registers:write-registers!))

(define (next-step)
  (step!)
  (refresh))

(define (prev-step)
  (step-back!)
  (refresh))

(define default-keymap
  (make-hash
   (list (cons #\q exit-tui)
         (cons #\Q exit-tui)

         (cons #\n next-step)
         (cons #\p prev-step))))

(define (handle-key key)
  (match (hash-ref (current-keymap) key #f)
    [#f
     (raise-tui-error 'handle-key "no keybinding for key: ~v" key)]
    [(? procedure? f)
     (f)]))
