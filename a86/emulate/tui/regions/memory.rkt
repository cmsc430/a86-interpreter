#lang racket

(require "../../../utility.rkt"
         "../../memory.rkt"
         "../../state.rkt"
         "../draw.rkt"
         "../region.rkt"
         "../state.rkt"
         "../term.rkt")

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
  #:height-spec  25  ;; 2 header + 21 rows + 2 border.

  (define-field stack-heading            " Stack")
  (define-field heap-heading              " Heap")
  (define-field static-heading   " Static Memory")
  (define-field border-style                'none)
  (define-field addr-col-width                 12)
  (define-field column-gap                      2)
  (define-field val-col-width                  20)
  (define-field current-stack-focus            #f)
  (define-field current-heap-focus             #f)
  (define-field current-static-focus           #f)
  (define-field subregion-width (+ 2
                                   addr-col-width
                                   column-gap
                                   val-col-width))

  (define-fields
    [header-row
     first-main-row
     content-height
     content-lo-idx
     content-hi-idx
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
       (set! content-height       (- this-region:height 2 2))
       (set! content-lo-idx       (- (floor (/ content-height 2))))
       (set! content-hi-idx       (floor (/ content-height 2)))
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
    (if (number? value)
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
           (~a value #:width val-col-width #:align 'right)])
        (~a value #:width val-col-width #:align 'right)))

  (define-method (format-row address value)
    (~a (format-address address)
        (make-string column-gap #\space)
        (format-value value)))

  (define-method (write-region! left-addr-col left-val-col center-addr downwards? select?)
    (term:with-saved-pos
     (let ([center-section (address-section-name (current-memory) center-addr)])
       (for ([row-offset (in-range content-height)]
             #;[addr-idx (in-inclusive-range content-lo-idx content-hi-idx)]
             [addr-idx (in-inclusive-range -2 2)])
         (let* ([addr (word-aligned-offset center-addr
                                           (* (if downwards? -1 1) addr-idx))]
                [addr-section (address-section-name (current-memory) addr)]
                [row (+ first-main-row row-offset)])
           (term:set-current-pos! left-addr-col row)
           (term:with-mode
            (if (and select? (eq? addr-idx 0)) 'inverse 'normal)
            (term:display #:width (- subregion-width 2)
                          (string-append
                           (format-address addr)
                           (make-string column-gap #\space)
                           (format-value
                            (if (eq? addr-section center-section)
                                (if (address-readable? (current-memory) addr)
                                    (current-memory-ref addr)
                                    "<unreadable>")
                                (~a "<." addr-section ">")))))))))))

  (define-method (write-stack!)
    (match current-stack-focus
      [(list addr new?)
       (write-region! stack-left-addr-col
                      stack-left-val-col
                      addr
                      (memq 'stack downward-sections)
                      new?)]
      [#f (void)]))

  (define-method (write-heap!)
    (match current-heap-focus
      [(list addr new?)
       (write-region! heap-left-addr-col
                      heap-left-val-col
                      addr
                      (memq 'heap downward-sections)
                      new?)]
      [#f (void)]))

  (define-method (write-static!)
    (match current-static-focus
      [(list addr new?)
       (write-region! static-left-addr-col
                      static-left-val-col
                      addr
                      (memq (address-section-name (current-memory) addr)
                            downward-sections)
                      new?)]
      [#f (void)]))

  (define-method (update-regions!)
    (set! current-stack-focus  (and current-stack-focus
                                    (list (car current-stack-focus) #f)))
    (set! current-heap-focus   (and current-heap-focus
                                    (list (car current-heap-focus) #f)))
    (set! current-static-focus (and current-static-focus
                                    (list (car current-static-focus) #f)))
    (for ([transaction (reverse (current-transactions))])
      (let ([address (transaction-destination transaction)])
        (match (address-section-name (current-memory) address)
          [(? (λ (x) (eq? x stack))) (set! current-stack-focus  (list address #t))]
          [(? (λ (x) (eq? x heap)))  (set! current-heap-focus   (list address #t))]
          [_                         (set! current-static-focus (list address #t))])))
    (write-stack!)
    (write-heap!)
    (write-static!))

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
         (term:set-mode-normal!)
         (update-regions!)])))))
