#lang racket

(provide (struct-out exn:fail:a86)
         make-exn:fail:a86
         raise-a86-error
         define-a86-exn/provide)

(require (for-syntax racket/string
                     racket/syntax
                     syntax/parse))

(struct exn:fail:a86 exn:fail ()
  #:extra-constructor-name make-exn:fail:a86
  #:transparent)

(define (raise-a86-error who format-str . args)
  (raise (exn:fail:a86
          (apply format
                 (string-append "~s: " format-str)
                 who
                 args)
          (current-continuation-marks))))

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
                    "raise-a86-~a-error"
                    (string-replace (symbol->string (syntax-e #'name))
                                    ":"
                                    "-")))

     #`(begin (provide (struct-out full-name)
                       raise-func-name)
              (struct full-name parent-struct-name (extra-fields ...)
                #:extra-constructor-name constructor-name
                #:transparent)
              (define (raise-func-name . args)
                (let-values ([(field-args other-args)
                              (split-at args #,(length (syntax->list #'(extra-fields ...))))])
                  (match other-args
                    [(list who)
                     (raise (apply constructor-name
                                   (~s who)
                                   (current-continuation-marks)
                                   field-args))]
                    [(list* who format-str args)
                     (raise (apply constructor-name
                                   (apply format
                                          (string-append "~s: " format-str)
                                          who
                                          args)
                                   (current-continuation-marks)
                                   field-args))]))))]))

(define-a86-exn/provide user ())
