#lang racket

(provide (struct-out exn:fail:a86)
         make-exn:fail:a86
         define-a86-exn/provide)

(require (for-syntax racket/string
                     racket/syntax
                     syntax/parse))

(struct exn:fail:a86 exn:fail ()
  #:extra-constructor-name make-exn:fail:a86
  #:transparent)

(define-syntax (define-a86-exn/provide stx)
  (syntax-parse stx
    [(_ name:id (~describe "fields" (extra-fields ...))
        (~optional (~seq #:parent-name parent-name:id))
        (~optional (~seq #:raise-error-name raise-error-name:id)))
     #:with full-name        (format-id #'name #:source #'name      "exn:fail:a86:~a" #'name)
     #:with constructor-name (format-id #'name #:source #'name "make-exn:fail:a86:~a" #'name)
     #:with parent-struct-name
     (if (attribute parent-name)
         #'parent-name
         (format-id #'name #:source #'name
                    (string-join (reverse (cdr (reverse (string-split
                                                         (symbol->string (syntax-e #'full-name))
                                                         ":"))))
                                 ":")))
     #:with raise-func-name
     (if (attribute raise-error-name)
         #'raise-error-name
         (format-id #'name #:source #'name
                    "raise-~a-error"
                    (string-replace (symbol->string (syntax-e #'name))
                                    ":"
                                    "-")))
     #'(begin (provide (struct-out full-name)
                       raise-func-name)
              (struct full-name parent-struct-name (extra-fields ...)
                #:extra-constructor-name constructor-name
                #:transparent)
              (define (raise-func-name who format-str . args)
                (raise (constructor-name
                        (apply format
                               (string-append "~s: " format-str)
                               who
                               args)
                        (current-continuation-marks)))))]))
