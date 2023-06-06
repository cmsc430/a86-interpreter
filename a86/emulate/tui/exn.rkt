#lang racket

(provide (struct-out exn:fail:tui)
         raise-tui-error)

;; TODO: This should be integrated into something like an [exn:fail:a86]
;; hierarchy or something.
(struct exn:fail:tui exn:fail ()
  #:extra-constructor-name make-exn:fail:tui
  #:transparent)

(define (raise-tui-error name format-str . args)
  (raise (make-exn:fail:tui
          (apply format
                 (string-append "~s: " format-str)
                 name
                 args)
          (current-continuation-marks))))
