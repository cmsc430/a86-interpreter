#lang racket

(provide open-tracking-input-string
         with-tracking-input-from-string)

;; A custom input port that keeps track of what's gone into it and how much has
;; been read from it. This allows for updating the REPL with information about
;; the current position within the input, and will also enable updating the
;; input on the fly.

(define (open-tracking-input-string str [name 'tracking-string])
  (let ([in-str (open-input-string str)])
    (make-input-port name
                     ;; read-in
                     (λ (bstr)
                       ;; exact-nonnegative-integer? :: number of bytes read
                       ;; eof-object?                :: end of input
                       #f)
                     ;; peek
                     ;; close
                     ;; [get-progress-evt #f]
                     ;; [commit           #f]
                     ;; [get-location     #f]
                     ;; [count-lines!     #f]
                     ;; [init-position     1]
                     ;; [buffer-mode      #f]
                     )))

(define (with-tracking-input-from-string str proc)
  (parameterize ([current-input-port (open-tracking-input-string str)])
    (proc)))
