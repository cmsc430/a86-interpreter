#lang racket

(require "../../../registers.rkt"
         "../draw.rkt"
         "../region.rkt"
         "../state.rkt"
         "../term.rkt")

(define-region registers
  #:width-spec  25  ;; 3 reg name + 18 max value + 2 gap + 2 border.
  #:height-spec 20  ;; 2 header + 16 registers + 2 border.

  (define-field border-style 'none)
  (define-field name-heading "Name")
  (define-field name-col-width 4)
  (define-field value-heading "Value")
  (define-field value-width     18)
  (define-field column-gap       2)
  (define-field settings (make-vector 16 'hex))

  (define-fields
    [header-row
     first-main-row
     left-name-col
     left-value-col])

  (define-post-init ()
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       (set! header-row (add1 from-y))
       (set! first-main-row (+ 2 header-row))
       (set! left-name-col (add1 from-x))
       (set! left-value-col (+ left-name-col
                               4
                               column-gap))]))

  (define-method (format-value value settings)
    (match settings
      ['hex
       (string-append
        "0x"
        (~r value #:base 16 #:min-width 16 #:pad-string "0"))]
      ['dec
       (~a value #:width 18 #:align 'right)]))

  (define-method (write-values!)
    (term:with-saved-pos
     (for ([register (in-list registers/64-bit)]
           [settings (in-vector settings)]
           [row (in-col first-main-row)]
           #:do [(define value (register-ref (current-registers) register))])
       (term:set-current-pos! left-value-col row)
       (term:display (format-value value settings)))))

  (define-method (redraw!)
    (term:with-saved-pos
     (term:with-mode
      'normal
      (match this-region:coords
        [(list from-x from-y to-x to-y)
         (draw:fill-box from-x from-y to-x to-y)
         (term:set-current-pos! left-name-col header-row)
         (term:with-mode
          'underline
          (term:display #:width (- this-region:width 2)
                        (string-append
                         (~a name-heading #:width name-col-width)
                         (make-string column-gap #\space)
                         (~a value-heading #:width value-width))))
         (for ([register-name (in-list registers/64-bit)]
               [index         (in-naturals)]
               [row           (in-col first-main-row)])
           (term:set-current-pos! left-name-col row)
           (term:display (~a register-name #:width name-col-width #:align 'right)))
         (write-values!)])))))
