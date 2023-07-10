#lang racket

(require "../../memory.rkt"
         "../draw.rkt"
         "../region.rkt"
         "../state.rkt")

;; ┌────────────────────────────────────────────────┐
;; │ Instructions (.text Section)                   │
;; │                                                │
;; │             [0x--------]  (Ret)                │
;; │    label_1  [0x--------]  (Call 'label_2)      │
;; │             [0x--------]  (Or 'rax 3)          │
;; │             [0x--------]  (Or 'rax 2)          │
;; │             [0x--------]  (Or 'rax 1)          │
;; │             [0x--------]  (Mov 'rax 'rbx)      │
;; │             [0x--------]  (Mov 'rbx 'rax)      │
;; │             [0x--------]  (Ret)                │
;; │             [0x--------]  (Mov 'rax 2)         │
;; │    label_2  [0x--------]  (Push 'rax)          │
;; │             [0x--------]  (Pop 'rax)           │
;; │             [0x--------]  (And 'rax 34)        │
;; │             [0x--------]  (Je 'label_1)        │
;; │->           [0x--------]  (Sub 'rax 1)         │
;; │             [0x--------]  (Add 'rax 1)         │
;; │             [0x--------]  (Jmp 'label_1)       │
;; └────────────────────────────────────────────────┘
;;
;; TODO: Should the window show + highlight the last instruction that ran? The
;; current setup is visually confusing because the highlighted flags/registers/
;; memory are relevant due to the previous instruction's execution, which may no
;; longer even be visible. Highlighting the last instruction would instead show
;; what caused the changes.
;;
;; If we had colors, the next instruction could be highlighted with a specific
;; color (if visible), which could solve everything. Not right now, though.
(define-region instructions
  #:width-spec  81
  #:height-spec 34

  (define-field border-style                 'none)
  (define-field heading " Instructions (.text Section)")
  (define-field select-indicator              "->")
  (define-field breakpoint-indicator          "**")
  ;; TODO: Replace individual indicators with this.
  (define-field indicators (hash 'select     "->"
                                 'breakpoint "**"))
  (define-field column-gap                       2)
  (define-field label-col-width                  0)
  (define-field max-label-width                  0)
  (define-field select-col-width                 2)
  (define-field max-addr-width                   0)
  (define-field addr-col-width                   0)
  (define-field max-instr-width                  0)
  (define-field instr-col-width                  0)

  (define-fields
    [instructions-list
     address->instruction-index
     address->label
     current-instr-idx
     instruction-window-lo
     instruction-window-hi
     header-row
     first-main-row
     last-main-row
     main-height
     third-main-height
     interior-third-top-edge-row
     interior-third-bottom-edge-row
     top-third-size
     interior-third-size
     bottom-third-size
     first-col
     left-select-col
     left-label-col
     left-addr-col
     left-instr-col])

  (define-pre-init ()
    (set! address->label
          (for/hash ([(label addr) (in-hash (current-labels->addresses))])
            (set! max-label-width
                  (max max-label-width
                       (string-length (~a label))))
            (values addr label)))
    (set! label-col-width max-label-width)
    (set! address->instruction-index (make-hash))
    (set! instructions-list
          (for/list ([addr+instr (in-memory-section (current-memory) text
                                                    #:order 'descending)]
                     [index (in-naturals)])
            (match addr+instr
              [(cons address instruction)
               (hash-set! address->instruction-index address index)
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
    ;; Refresh all the internal size definitions.
    (update-refs!)
    ;; Set the initial instruction selection and window.
    (set! current-instr-idx     0)
    (set! instruction-window-lo 0)
    (set! instruction-window-hi (min (sub1 main-height)
                                     (sub1 (length instructions-list)))))

  (define-method (format-label label [width max-label-width])
    (~a label #:width width))

  (define-method (format-addr addr [width max-addr-width])
    (format "[0x~a]" (~r addr #:base 16 #:min-width width #:pad-string "0")))

  (define-method (format-instr instr [width max-instr-width])
    (~v instr #:width width))

  (define-method (update-refs!)
    (match this-region:coords
      [(list from-x from-y to-x to-y)
       ;; Set column sizes.
       (set! first-col       (add1 from-x))
       (set! left-select-col first-col)
       (set! left-label-col  (+ left-select-col
                                select-col-width
                                column-gap))
       (set! left-addr-col   (+ left-label-col
                                label-col-width
                                column-gap))
       (set! left-instr-col  (+ left-addr-col
                                addr-col-width
                                column-gap))
       ;; Calculate row information.
       (set! header-row     (add1 from-y))
       (set! first-main-row (+ 2 header-row))
       (set! last-main-row  (+ first-main-row (- to-y first-main-row 1)))
       (set! main-height    (add1 (- last-main-row first-main-row)))
       (let-values ([(third-size rem) (quotient/remainder main-height 3)])
         (set! interior-third-top-edge-row    (+ first-main-row
                                                 third-size))
         (set! interior-third-bottom-edge-row (+ interior-third-top-edge-row
                                                 third-size
                                                 (sub1 rem)))
         (set! top-third-size            (- interior-third-top-edge-row
                                            first-main-row))
         (set! interior-third-size (add1 (- interior-third-bottom-edge-row
                                            interior-third-top-edge-row)))
         (set! bottom-third-size         (- last-main-row
                                            interior-third-bottom-edge-row))
         (set! third-main-height third-size))]))

  (define-method (write-to-label-col row text)
    (term:with-saved-pos
     (term:set-current-pos! left-label-col row)
     (term:display #:width label-col-width text)))

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
     (term:set-current-pos! first-col header-row)
     (term:with-mode
      'underline
      (term:display #:width (- this-region:width 2) heading))))

  (define-method (write-row! row selected? addr instr)
    (term:with-saved-pos
     (term:set-current-pos! left-select-col row)
     ;; The row is written in ['inverse] mode if it is "selected", otherwise it
     ;; is written in the ['normal] mode.
     (term:with-mode
      (if selected? 'inverse 'normal)
      (term:display #:width (- this-region:width 2)
                    (string-append
                     (if selected?
                         select-indicator
                         (make-string select-col-width #\space))
                     (make-string column-gap #\space)
                     (format-label (hash-ref address->label addr ""))
                     (make-string column-gap #\space)
                     (format-addr addr)
                     (make-string column-gap #\space)
                     (format-instr instr))))))

  (define-method (refresh-state!)
    ;; 1. Figure out which instruction in the instructions list should be
    ;;    selected next.
    ;; 2. Determine whether the selection window needs to be updated.
    ;;
    ;;    a. If it does, calculate the new selection window and redraw.
    ;;
    ;;    b. If it does not...
    ;;
    ;;        i. Rewrite the currently selected row as a not-selected row.
    ;;       ii. Rewrite the newly selected row as a selected row.
    (let* ([curr-addr+instr (list-ref instructions-list current-instr-idx)]
           [curr-instr-row (+ first-main-row
                              (- current-instr-idx instruction-window-lo))]
           [next-instr-idx (hash-ref address->instruction-index
                                     (current-instruction-pointer))]
           [next-addr+instr (list-ref instructions-list next-instr-idx)]
           [instr-idx-diff (- next-instr-idx current-instr-idx)]
           [in-top-third? (λ (r) (<= first-main-row r (sub1 interior-third-top-edge-row)))]
           [in-int-third? (λ (r) (<= interior-third-top-edge-row r interior-third-bottom-edge-row))]
           [in-bot-third? (λ (r) (<= (add1 interior-third-bottom-edge-row) r last-main-row))]
           ;; NOTE: This new instruction row might be outside the range of the
           ;; instruction window. It may need to be adjusted later.
           [new-instr-row (+ curr-instr-row instr-idx-diff)]
           ;; By default, we assume we'll have to redraw.
           [redraw? #t])
      ;; Determine where the new instruction will be and update the instruction
      ;; window accordingly.
      ;;
      ;; Terminology:
      ;;
      ;;   NI    New Instruction
      ;;   PI    Previous Instruction
      ;;   T1    Top third
      ;;   T2    Interior third
      ;;   T3    Bottom third
      (cond
        ;; In some conditions, we don't shift:
        ;;
        ;;   * Fewer instructions than the height of the window.
        ;;   * NI is already in T2.
        ;;   * NI and PI are in T1.
        [(or (<= (length instructions-list) main-height)
             (in-int-third? new-instr-row)
             (and (in-top-third? curr-instr-row)
                  (in-top-third? new-instr-row)))
         ;; Do not redraw the whole window.
         (set! redraw? #f)]
        ;; NI is not in T1 but is above T2. Shift the window to lower indices.
        [(< new-instr-row interior-third-top-edge-row)
         ;; We prefer to shift the window such that NI is just inside T2, but we
         ;; will not leave blank space at the top of the window.
         (if (>= next-instr-idx top-third-size)
             ;; We can shift NI into T2.
             (let ([shift-diff (- new-instr-row interior-third-top-edge-row)])
               (set! instruction-window-lo (+ instruction-window-lo shift-diff))
               (set! instruction-window-hi (+ instruction-window-hi shift-diff))
               (set! new-instr-row interior-third-top-edge-row))
             ;; We can't shift the window all the way. The lower bound of the
             ;; window is 0.
             (begin
               (set! instruction-window-lo 0)
               (set! instruction-window-hi (sub1 main-height))
               (set! new-instr-row (+ first-main-row next-instr-idx))))]
        ;; NI is not in T3 but is below T2. Shift the window to greater indices.
        [(> new-instr-row interior-third-bottom-edge-row)
         ;; We prefer to shift the window such that NI is just inside T2, but we
         ;; will not leave blank space at the bottom of the window.
         (if (>= (- (sub1 (length instructions-list)) next-instr-idx)
                 bottom-third-size)
             ;; We can shift NI into T2.
             (let ([shift-diff (- new-instr-row interior-third-bottom-edge-row)])
               (set! instruction-window-lo (+ instruction-window-lo shift-diff))
               (set! instruction-window-hi (+ instruction-window-hi shift-diff))
               (set! new-instr-row interior-third-bottom-edge-row))
             ;; We can't shift the window all the way. The upper bound of the
             ;; window is the last index in the list of instructions.
             (begin
               (set! instruction-window-hi (sub1 (length instructions-list)))
               (set! instruction-window-lo (- instruction-window-hi (sub1 main-height)))
               (set! new-instr-row (- last-main-row
                                      (- instruction-window-hi next-instr-idx)))))]
        [else
         (error 'refresh-state! "could not determine new instruction location")])
      ;; Correct if shifts are too far.
      (set! instruction-window-lo (max instruction-window-lo 0))
      (set! instruction-window-hi (min instruction-window-hi (sub1 (length instructions-list))))
      ;; Update the current information.
      (set! current-instr-idx next-instr-idx)
      ;; Redraw if necessary.
      (if redraw?
          (redraw!)
          (begin
            ;; Swap the selection of the rows.
            (write-row! curr-instr-row #f (car curr-addr+instr) (cdr curr-addr+instr))
            (write-row! new-instr-row #t (car next-addr+instr) (cdr next-addr+instr))))))

  (define-method (redraw!)
    (term:with-saved-pos
     (match this-region:coords
       [(list from-x from-y to-x to-y)
        (draw:fill-box from-x from-y to-x to-y)
        (draw:border from-x from-y to-x to-y #:style border-style)
        (write-header!)
        (for ([idx (in-inclusive-range instruction-window-lo
                                       instruction-window-hi)]
              [row (in-naturals first-main-row)]
              #:do [(define addr+instr (list-ref instructions-list idx))
                    (define addr (car addr+instr))
                    (define instr (cdr addr+instr))
                    (define selected? (= idx current-instr-idx))])
          (write-row! row selected? addr instr))]))))
