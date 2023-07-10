#lang racket

(require "../draw.rkt"
         "../region.rkt"
         "../state.rkt"
         "../term.rkt")

;; what look like
;;
;; TODO: probably need the ability to get step-indexed memory
;;
;; modes:
;;   - show everything in hex
;;   - show everything in decimal
;;   - show only written-to addresses in hex
;;   - show only written-to addresses in decimal
;;
;; it'd be cool to support a custom map for specific values or predicates, so
;; they would be rendered in a particular way, e.g.
;;
;;                          152  ---  "'()"
;;                            4  ---  "1"
;;   (λ (x) (ends-in? x 0b010))  ---  "cons"

;; ┌────────────────────────────────┐┌────────────────────────────────┐┌────────────────────────────────┐
;; │ Stack                          ││ Heap                           ││ Static Memory                  │
;; │                                ││                                ││                                │
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; │[0x--------]  0x----------------││[0x--------]  0x----------------││[0x--------]  0x----------------│
;; └────────────────────────────────┘└────────────────────────────────┘└────────────────────────────────┘
(define-region memory
  #:width-spec  108  ;; 3 x (2 border + 12 address + 2 gap + 20 value).
  #:height-spec   9  ;; 2 header + 5 rows + 2 border.

  (define-field stack-heading          " Stack")
  (define-field heap-heading            " Heap")
  (define-field static-heading " Static Memory")
  (define-field border-style              'none)
  (define-field addr-col-width               12)
  (define-field column-gap                    2)
  (define-field val-col-width                20)
  (define-field subregion-width (+ 2
                                   addr-col-width
                                   column-gap
                                   val-col-width))

  (define-fields
    [header-row
     first-main-row
     stack-left-addr-col
     stack-left-val-col
     heap-left-addr-col
     heap-left-val-col
     static-left-addr-col
     static-left-val-col])

  (define-post-init ()
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       (set! header-row           (add1 from-y))
       (set! first-main-row       (+ 2 header-row))
       (set! stack-left-addr-col  (add1 from-x))
       (set! stack-left-val-col   (+ stack-left-addr-col
                                     addr-col-width
                                     column-gap))
       (set! heap-left-addr-col   (+ stack-left-val-col
                                     val-col-width
                                     2))
       (set! heap-left-val-col    (+ heap-left-addr-col
                                     addr-col-width
                                     column-gap))
       (set! static-left-addr-col (+ heap-left-val-col
                                     val-col-width
                                     2))
       (set! static-left-val-col  (+ static-left-addr-col
                                     addr-col-width
                                     column-gap))]))

  (define-method (format-address address [setting 'hex])
    (match setting
      ['hex
       (string-append "[0x"
                      (~a (~r address
                              #:base 16
                              #:min-width (- addr-col-width 4)
                              #:pad-string "0")
                          #:width (- addr-col-width 4)
                          #:limit-marker "*"
                          #:limit-prefix? #t)
                      "]")]
      ['dec
       (string-append "["
                      (~a (~r address
                              #:base 10
                              #:min-width (- addr-col-width 2)
                              #:pad-string "0")
                          #:width (- addr-col-width 2)
                          #:limit-marker "*"
                          #:limit-prefix? #t)
                      "]")]))

  (define-method (format-value value [setting 'hex])
    (match setting
      ['hex
       (~a "0x"
           (~r value
               #:base 16
               #:min-width 16
               #:pad-string "0")
           #:width val-col-width
           #:align 'right)]
      ['dec
       (~a value #:width val-col-width #:align 'right)]))

  (define-method (redraw!)
    (term:with-saved-pos
     (term:with-mode
      'normal
      (match this-region:coords
        [(list from-x from-y to-x to-y)
         (draw:fill-box from-x from-y to-x to-y)
         (term:set-mode-underline!)
         (term:set-current-pos! stack-left-addr-col header-row)
         (term:display #:width (- subregion-width 2) stack-heading)
         (term:set-current-col! heap-left-addr-col)
         (term:display #:width (- subregion-width 2) heap-heading)
         (term:set-current-col! static-left-addr-col)
         (term:display #:width (- subregion-width 2) static-heading)
         (term:set-mode-normal!)])))))
