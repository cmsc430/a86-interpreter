#lang racket

(require "../memory.rkt"
         "draw.rkt"
         "region.rkt"
         "state.rkt")

(define-region header
  #:width-spec total
  #:height-spec 3

  (define-fields ())

  (define-method (redraw!)
    (term:with-saved-pos
     (match this-region:coords
       [(list from-x from-y to-x to-y)
        (draw:fill-box from-x from-y to-x to-y)
        (term:set-current-pos! (add1 from-x) (add1 from-y))
        (term:display "HEADER")]))))

(define-region instructions
  #:width-spec  25%
  #:height-spec available

  (define-field border-style      'none)
  (define-field column-gap        2)
  (define-field addr-heading      "Addr")
  (define-field instr-heading     "Instr")
  (define-field select-indicator  " -> ")
  (define-field select-col-width  (string-length select-indicator))
  (define-field max-addr-width    0)
  (define-field addr-col-width    0)
  (define-field max-instr-width   0)
  (define-field instr-col-width   0)
  (define-field current-instr-idx 0)

  (define-fields
    (instructions-list
     instruction-selection-window-lo
     instruction-selection-window-hi
     header-row first-main-row
     left-select-col left-addr-col left-instr-col))

  (define-method (format-addr addr [width #f])
    (unless width
      (set! width (if (positive-integer? max-addr-width)
                      max-addr-width
                      1)))
    (format "[0x~a]" (~r addr #:base 16 #:min-width width #:pad-string "0")))

  (define-method (format-instr instr [width #f])
    (~v instr #:width width))

  (define-method (update-refs!)
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       (set! header-row      (add1 from-y))
       (set! first-main-row  (+ 2 header-row))
       (set! left-select-col (add1 from-x))
       (set! left-addr-col   (+ left-select-col
                                select-col-width))
       (set! left-instr-col  (+ left-addr-col
                                addr-col-width
                                column-gap))]))

  (define-method (update-instruction-selection-window!)
    (let* ([instr-rows (- this-region:height 2 1)]
           [instr-count (length instructions-list)]
           [third-count (quotient instr-rows 3)])
      (let-values
          ([(lo hi)
            (cond
              ;; FIXME: Not implemented! Needs to compute around the currently
              ;; selected instruction!
              [(<= instr-count instr-rows)
               (values 0 (sub1 instr-count))]
              [else
               (values 0 (- instr-rows 3))])])
        (set! instruction-selection-window-lo lo)
        (set! instruction-selection-window-hi hi))))

  (define-method (write-to-addr-col row text)
    (term:with-saved-pos
     (term:set-current-pos! left-addr-col row)
     (term:display #:width addr-col-width text)))

  (define-method (write-to-instr-col row text)
    (term:with-saved-pos
     (term:set-current-pos! left-instr-col row)
     (term:display #:width instr-col-width text)))

  (define-method (write-header!)
    (term:with-saved-pos
     (term:set-current-pos! left-select-col header-row)
     (term:display #:underline
                   #:width (- this-region:width 2)
                   (string-append
                    (make-string select-col-width #\space)
                    (~a addr-heading #:width addr-col-width)
                    (make-string column-gap #\space)
                    (~a instr-heading #:width instr-col-width)))))

  (define-method (write-row! row selected? addr instr)
    (term:with-saved-pos
     (term:set-current-pos! left-select-col row)
     (term:set-mode-normal)
     (if selected?
         (begin
           (term:display select-indicator)
           (term:set-mode-inverse))
         (term:display (make-string select-col-width #\space)))
     (term:display #:width (- this-region:width 2 select-col-width)
                   (string-append
                    (format-addr addr)
                    (make-string column-gap #\space)
                    (format-instr instr)))))

  (define-method (select-instr! instr-idx)
    (set! current-instr-idx instr-idx)
    (redraw!))

  (define-method (redraw!)
    (term:with-saved-pos
     (match this-region:coords
       [(list from-x from-y to-x to-y)
        (draw:fill-box from-x from-y to-x to-y)
        (write-header!)
        (for ([idx (in-inclusive-range instruction-selection-window-lo
                                       instruction-selection-window-hi)]
              [row (in-naturals (add1 header-row))]
              #:do [(define addr+instr (list-ref instructions-list idx))
                    (define addr (car addr+instr))
                    (define instr (cdr addr+instr))
                    (define selected? (= idx current-instr-idx))])
          (write-row! row selected? addr instr))])))

  (define-pre-init ()
    (set! instructions-list
          (for/list ([addr+instr (in-memory-section (current-memory) text)])
            (match addr+instr
              [(cons address instruction)
               (set! max-addr-width
                     (max max-addr-width
                          (string-length (~r address #:base 16))))
               (set! max-instr-width
                     (max max-instr-width
                          (string-length (~v instruction))))
               addr+instr])))
    (set! addr-col-width (+ 4 max-addr-width))
    (set! instr-col-width max-instr-width))

  (define-post-init ()
    (update-refs!)
    (update-instruction-selection-window!)
    (redraw!)))
