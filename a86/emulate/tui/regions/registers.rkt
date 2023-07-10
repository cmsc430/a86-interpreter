#lang racket

(require "../../../registers.rkt"
         "../draw.rkt"
         "../region.rkt"
         "../state.rkt"
         "../term.rkt")

;; ┌───────────────────────┐
;; │ Flags                 │
;; │                       │
;; │    OF  -     SF  -    │
;; │    ZF  -     CF  -    │
;; │                       │
;; │ Registers             │
;; │                       │
;; │rax  0x----------------│
;; │...  ..................│  <- 14 rows omitted for brevity
;; │r15  0x----------------│
;; └───────────────────────┘
;;
;; To calculate flag value position:
;;
;;
(define-region registers
  #:width-spec  27  ;; 3 reg name + 2 gap + 18 max value + 2 gap + 2 border.
  #:height-spec 25  ;; 4 (flags header + 2 rows) 18 (regs header + rows) + 1 gap
                    ;; + 2 border.

  (define-field flags-heading         " Flags")
  (define-field regs-heading      " Registers")
  (define-field border-style             'none)
  (define-field flag-name-col-width          2)
  (define-field regs-name-col-width          3)
  (define-field value-width                 18)
  (define-field column-gap                   2)
  (define-field settings (make-vector 16 'hex))

  (define-fields
    [flags-header-row
     regs-header-row
     first-main-flags-row
     last-main-flags-row
     left-flag-name-col1
     left-flag-value-col1
     left-flag-name-col2
     left-flag-value-col2
     first-main-regs-row
     left-name-col
     left-value-col])

  (define-post-init ()
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       (set! flags-header-row     (add1 from-y))
       (set! first-main-flags-row (+ 2 flags-header-row))
       (set! last-main-flags-row  (+ first-main-flags-row
                                     (ceiling (/ (length flag-names) 2))
                                     -1))
       (set! left-flag-name-col1  (+ from-x 6))
       (set! left-flag-value-col1 (+ left-flag-name-col1
                                     flag-name-col-width
                                     column-gap))
       (set! left-flag-name-col2  (+ left-flag-value-col1
                                     1 column-gap
                                     1 column-gap))
       (set! left-flag-value-col2 (+ left-flag-name-col2
                                     flag-name-col-width
                                     column-gap))
       (set! regs-header-row      (+ 2 last-main-flags-row))
       (set! first-main-regs-row  (+ 2 regs-header-row))
       (set! left-name-col        (add1 from-x))
       (set! left-value-col       (+ left-name-col
                                     regs-name-col-width
                                     column-gap))]))

  (define-method (format-value value settings)
    (match settings
      ['hex
       (string-append
        "  0x"
        (~r value #:base 16 #:min-width 16 #:pad-string "0"))]
      ['dec
       (~a value #:width 20 #:align 'right)]))

  (define-method (write-flags!)
    (term:with-saved-pos
     (for ([flag (in-list flag-names)]
           [idx (in-naturals 0)]
           #:do [(define-values (q r) (quotient/remainder idx 2))
                 (define col (match r
                               [0 left-flag-value-col1]
                               [1 left-flag-value-col2]))
                 (define row (+ first-main-flags-row q))
                 (define curr-value (current-flag-ref flag))
                 (define prev-value (previous-flag-ref flag))
                 (define new-value? (and (not (eq? prev-value 'no-result))
                                         (not (eq? prev-value curr-value))))])
       (term:set-current-pos! col row)
       (term:with-mode
        (if new-value? 'inverse 'normal)
        (term:display (if curr-value 1 0))))))

  (define-method (write-registers!)
    (term:with-saved-pos
     (for ([register (in-list registers/64-bit)]
           [settings (in-vector settings)]
           [row (in-col first-main-regs-row)]
           #:do [(define curr-value (current-register-ref register))
                 (define prev-value (previous-register-ref register))
                 (define new-value? (and prev-value
                                         (not (eq? prev-value curr-value))))])
       (term:set-current-pos! left-value-col row)
       (term:with-mode
        (if new-value? 'inverse 'normal)
        (term:display (format-value curr-value settings))))))

  (define-method (redraw!)
    (term:with-saved-pos
     (term:with-mode
      'normal
      (match this-region:coords
        [(list from-x from-y to-x to-y)
         (draw:fill-box from-x from-y to-x to-y)
         (term:set-current-pos! left-name-col flags-header-row)
         (term:with-mode
          'underline
          (term:display #:width (- this-region:width 2) flags-heading))
         (term:set-current-pos! left-name-col regs-header-row)
         (term:with-mode
          'underline
          (term:display #:width (- this-region:width 2) regs-heading))
         (for ([flag-name (in-list flag-names)]
               [index     (in-naturals 0)]
               #:do [(define-values (q r) (quotient/remainder index 2))
                     (define col (match r
                                   [0 left-flag-name-col1]
                                   [1 left-flag-name-col2]))
                     (define row (+ first-main-flags-row q))])
           (term:set-current-pos! col row)
           (term:display (~a flag-name #:width flag-name-col-width)))
         (for ([register-name (in-list registers/64-bit)]
               [index         (in-naturals)]
               [row           (in-col first-main-regs-row)])
           (term:set-current-pos! left-name-col row)
           (term:display (~a register-name #:width regs-name-col-width)))
         (write-flags!)
         (write-registers!)])))))
