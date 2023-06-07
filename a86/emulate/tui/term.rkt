#lang racket

(provide ;; Read from state.
         term:screen-size
         term:col-count
         term:row-count
         term:current-pos
         term:current-col
         term:current-row
         term:last-key
         ;; Update state.
         term:set-current-col!
         term:set-current-row!
         term:set-current-pos!
         term:set-screen-size!
         term:get-key!
         ;; For iterating over the positions.
         in-term
         in-row
         in-col
         ;; Initialize loop.
         with-term
         term:with-loop
         ;; Write to the screen.
         term:set-mode-normal!
         term:set-mode-inverse!
         term:set-mode-underline!
         term:set-mode-blink!
         term:set-mode-bold!
         term:with-mode
         term:display
         term:displayln
         ;; Easily perform operations without adjusting the position for the
         ;; surrounding context.
         term:with-saved-pos

         (rename-out [current-term-state                 term:state]
                     ;; Original functions to re-export.
                     [charterm:charterm-clear-screen     term:clear-screen]
                     [charterm:charterm-clear-line       term:clear-line]
                     [charterm:charterm-clear-line-left  term:clear-line-left]
                     [charterm:charterm-clear-line-right term:clear-line-right]))

(require racket/stxparam

         (prefix-in charterm: charterm)

         (for-syntax racket/stxparam
                     syntax/parse))

(struct term-state
  (col-count
   row-count
   current-col
   current-row
   last-key
   write-mode)
  #:mutable)

(define current-term-state (make-parameter #f))

(define (term:make-state col-count row-count)
  (term-state col-count row-count 1 1 #f 'normal))

(define (term:col-count)
  (term-state-col-count (current-term-state)))

(define (term:row-count)
  (term-state-row-count (current-term-state)))

(define (term:screen-size)
  (let ([ts (current-term-state)])
    (values (term-state-col-count ts)
            (term-state-row-count ts))))

(define (term:current-col)
  (term-state-current-col (current-term-state)))

(define (term:current-row)
  (term-state-current-row (current-term-state)))

(define (term:current-pos)
  (let ([ts (current-term-state)])
    (values (term-state-current-col ts)
            (term-state-current-row ts))))

(define (term:last-key)
  (term-state-last-key (current-term-state)))

(define (term:set-screen-size! cols rows)
  (let ([ts (current-term-state)])
    (set-term-state-col-count! ts cols)
    (set-term-state-row-count! ts rows)))

(define (term:set-current-col! x)
  (term:set-current-pos! x #f))

(define (term:set-current-row! y)
  (term:set-current-pos! #f y))

(define (term:set-current-pos! x y)
  (let* ([ts (current-term-state)]
         [x (or x (term-state-current-col ts))]
         [y (or y (term-state-current-row ts))])
    (set-term-state-current-col! ts x)
    (set-term-state-current-row! ts y)
    (charterm:charterm-cursor x y)))

(define (term:get-key!)
  (let ([key (charterm:charterm-read-key)])
    (set-term-state-last-key! (current-term-state) key)
    key))

(define (in-term [x 1] [y 1])
  (let-values ([(screen-cols screen-rows) (term:screen-size)])
    (make-do-sequence
     (thunk
       (values
        ;; pos -> value
        (λ (pos) (values (car pos) (cdr pos)))
        ;; pos -> next pos
        (λ (pos)
          (if (= (car pos) screen-cols)
              (cons 1 (add1 (cdr pos)))
              (cons (add1 (car pos)) (cdr pos))))
        ;; initial value
        (cons x y)
        ;; pos -> okay?
        (λ (pos)
          (not (> (cdr pos) screen-rows)))
        ;; unneeded
        #f #f)))))

(define (in-row [lo-x #f] [hi-x #f] #:reversed? [reversed? #f])
  (set! lo-x (or lo-x 1))
  (set! hi-x (or hi-x (term:col-count)))
  (make-do-sequence
   (thunk
    (values
     (λ (x) x)
     (λ (x) ((if reversed? sub1 add1) x))
     (if reversed? hi-x lo-x)
     (λ (x) (if reversed?
                (>= x lo-x)
                (<= x hi-x)))
     #f #f))))

(define (in-col [lo-y #f] [hi-y #f] #:reversed? [reversed? #f])
  (set! lo-y (or lo-y 1))
  (set! hi-y (or hi-y (term:row-count)))
  (make-do-sequence
   (thunk
    (values
     (λ (y) y)
     (λ (y) ((if reversed? sub1 add1) y))
     (if reversed? hi-y lo-y)
     (λ (y) (if reversed?
                (>= y lo-y)
                (<= y hi-y)))
     #f #f))))

(define-syntax-parameter break
  (λ (stx)
    (raise-syntax-error #f "Cannot 'break' outside TUI loop" stx)))

(define-syntax (with-term stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(charterm:with-charterm
        (charterm:charterm-clear-screen)
        (let-values ([(num-cols num-rows) (charterm:charterm-screen-size)])
          (parameterize ([current-term-state (term:make-state num-cols num-rows)])
            body ...)))]))

(define-syntax (term:with-loop stx)
  (syntax-parse stx
    [(_ (loop-id break-id) body ...+)
     #'(charterm:with-charterm
        (charterm:charterm-clear-screen)
        (let-values ([(num-cols num-rows)
                      (charterm:charterm-screen-size)])
          (parameterize ([current-term-state (term:make-state num-cols num-rows)])
            (let/ec break-id
              (let loop-id ()
                (syntax-parameterize ([break (syntax-rules () [(_) (break-id)])])
                  body ...))))))]))

(define write-mode->mode-set-func
  (hash 'normal    charterm:charterm-normal
        'inverse   charterm:charterm-inverse
        'underline charterm:charterm-underline
        'blink     charterm:charterm-blink
        'bold      charterm:charterm-bold))
(define (get-set-mode-func mode)
  (match (hash-ref write-mode->mode-set-func mode #f)
    [#f (error 'term:set-mode! "not a recognized write mode: ~v" mode)]
    [set-mode set-mode]))
(define (term:set-mode! mode)
  (let ([set-mode (get-set-mode-func mode)])
    (set-term-state-write-mode! (current-term-state) mode)
    (set-mode)))
(define (term:set-mode-normal!)    (term:set-mode! 'normal))
(define (term:set-mode-inverse!)   (term:set-mode! 'inverse))
(define (term:set-mode-underline!) (term:set-mode! 'underline))
(define (term:set-mode-blink!)     (term:set-mode! 'blink))
(define (term:set-mode-bold!)      (term:set-mode! 'bold))

(define-syntax (term:with-mode stx)
  (syntax-parse stx
    [(_ mode body ... final-body)
     #'(let ([original-mode (term-state-write-mode (current-term-state))])
         (term:set-mode! mode)
         body ...
         (begin0 final-body
           (term:set-mode! original-mode)))]))

(define-syntax (term:display stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:width width)
                   #:defaults ([width #'#f]))
        str-expr
        ~rest fmt-args)
     (let ([fmt-args (syntax->list (attribute fmt-args))])
       #`(charterm:charterm-display
          #:width width
          #,(if (zero? (length fmt-args))
                #'str-expr
                #`(format str-expr #,@fmt-args))))]))

(define-syntax (term:displayln stx)
  (syntax-parse stx
    [(_ args ...)
     #'(let-values ([(orig-x orig-y) (term:current-pos)])
         (term:display args ...)
         (term:set-current-pos! orig-x (add1 orig-y)))]))

(define-syntax (term:with-saved-pos stx)
  (syntax-parse stx
    [(_ body ... final-body)
     #'(let-values ([(orig-x orig-y) (term:current-pos)])
         body ...
         (begin0 final-body
           (term:set-current-pos! orig-x orig-y)))]))
