#lang racket

(provide (struct-out posn)

         posn-with-col
         posn-with-row

         add-cols
         sub-cols
         add-rows
         sub-rows
         next-col
         prev-col
         next-row
         prev-row

         posn-col-map
         posn-row-map
         posn-map

         posn-bind

         posn+
         posn-
         posn*
         posn/
         posn=?
         posn<?
         posn<=?
         posn>?
         posn>=?

         posn-col+
         posn-col-
         posn-col*
         posn-col/
         posn-col=?
         posn-col<?
         posn-col<=?
         posn-col>?
         posn-col>=?

         posn-row+
         posn-row-
         posn-row*
         posn-row/
         posn-row=?
         posn-row<?
         posn-row<=?
         posn-row>?
         posn-row>=?

         in-col
         in-row

         unit-o
         unit-n
         unit-w
         unit-s
         unit-e
         unit-nw
         unit-ne
         unit-sw
         unit-se

         go-n
         go-w
         go-s
         go-e
         go-nw
         go-ne
         go-sw
         go-se

         cardinal?
         ordinal?
         direction?
         get-unit)

(require (for-syntax racket/list
                     racket/match
                     racket/string
                     racket/syntax
                     syntax/parse))

(struct posn (x y) #:transparent)

;; Functions for modifying positions more directly.
(define posn-with-col (match-lambda** [((posn _ y) (? exact-integer? x)) (posn x y)]
                                      [((posn _ y) (posn x _))           (posn x y)]))
(define posn-with-row (match-lambda** [((posn x _) (? exact-integer? y)) (posn x y)]
                                      [((posn x _) (posn _ y))           (posn x y)]))

;; These functions are only used internally.
(define adj-cols (match-lambda** [(op (posn x y) (? exact-integer? n))  (posn (op x n)     y   )]))
(define adj-rows (match-lambda** [(op (posn x y) (? exact-integer? n))  (posn     x    (op y n))]))

;; We define a set of functions for easily modifying given posnitions or the
;; current position.
(define add-cols (match-lambda* [(list p n) (adj-cols + p n)]))
(define sub-cols (match-lambda* [(list p n) (adj-cols - p n)]))
(define add-rows (match-lambda* [(list p n) (adj-rows + p n)]))
(define sub-rows (match-lambda* [(list p n) (adj-rows - p n)]))
(define next-col (match-lambda* [(list p)   (add-cols p 1)]))
(define prev-col (match-lambda* [(list p)   (sub-cols p 1)]))
(define next-row (match-lambda* [(list p)   (add-rows p 1)]))
(define prev-row (match-lambda* [(list p)   (sub-rows p 1)]))

;; These functions are used for manipulating collections of positions.
(define posn-col-map (match-lambda* [(list op (posn xs  _) ...)        (apply op xs)               ]))
(define posn-row-map (match-lambda* [(list op (posn  _ ys) ...)                      (apply op ys) ]))
(define posn-map     (match-lambda* [(list op (posn xs ys) ...)  (posn (apply op xs) (apply op ys))]))

;; Allows converting a position to something else.
(define posn-bind (match-lambda* [(list op (posn x y))  (op x y)]))

;; Manipulating positions is useful, so we provide many functions for it.
(define-syntax (define-posn-funcs stx)
  (define-syntax-class posn-func
    #:attributes (name op sub-type bool?)
    (pattern e:id
             #:with name #'e
             ;;        1          2       3      4   5
             ;; '(full-match base-name col/row? op huh?)
             #:attr name-parts (regexp-match
                                #px"^([\\w-]+?)(-col|-row)?([!@#/$%^&*+=:<>-]+)([?])?$"
                                (symbol->string (syntax-e #'name)))
             #:fail-when (and (not (attribute name-parts)) #'name) "invalid posn-func name"
             ;; Extract the parts of the regexp.
             #:attr op-str (fourth (attribute name-parts))
             #:attr sub-str (third (attribute name-parts))
             #:attr bool?   (fifth (attribute name-parts))
             ;; Convert the extracted strings to useful data.
             #:with op (datum->syntax #'e (string->symbol (attribute op-str)))
             #:attr sub-type (match (attribute sub-str)
                               ["-col" 'col]
                               ["-row" 'row]
                               [_        #f])))
  (syntax-parse stx
    [(_ (func:posn-func ...))
     #:with (def ...)
     (map (λ (name-stx bool? sub-type op-stx)
            (define inner-func-stx
              #`(apply
                 #,(match sub-type
                     ['col #'posn-col-map]
                     ['row #'posn-row-map]
                     [#f   #'posn-map])
                 #,op-stx
                 ps))
            #`(define (#,name-stx . ps)
                #,(if (and (not sub-type) bool?)
                      #`(match #,inner-func-stx
                          [(posn x-res y-res)
                           (and x-res y-res)])
                      inner-func-stx)))
          (attribute func.name)
          (attribute func.bool?)
          (attribute func.sub-type)
          (attribute func.op))
     #'(begin def ...)]))

(define-posn-funcs
  [posn+
   posn-
   posn*
   posn/
   posn=?
   posn<?
   posn<=?
   posn>?
   posn>=?
   posn-col+
   posn-col-
   posn-col*
   posn-col/
   posn-col=?
   posn-col<?
   posn-col<=?
   posn-col>?
   posn-col>=?
   posn-row+
   posn-row-
   posn-row*
   posn-row/
   posn-row=?
   posn-row<?
   posn-row<=?
   posn-row>?
   posn-row>=?])

(define ((make-inclusive-posn-dim-range-stream posn-with-dim posn-select-dim)
         p1 p2 [step 1])
  (stream-map (λ (v) (posn-with-dim p1 v))
              (in-inclusive-range (posn-select-dim p1)
                                  (posn-select-dim p2)
                                  step)))

(define in-col (make-inclusive-posn-dim-range-stream posn-with-row posn-y))
(define in-row (make-inclusive-posn-dim-range-stream posn-with-col posn-x))

(define-syntax (define-unit-posn stx)
  (syntax-parse stx
    [(_ dims:id)
     #:attr dims-str (symbol->string (syntax-e #'dims))
     #:with col (cond
                  [(string-contains? (attribute dims-str) "w") #'-1]
                  [(string-contains? (attribute dims-str) "e") #'1]
                  [else #'0])
     #:with row (cond
                  [(string-contains? (attribute dims-str) "n") #'-1]
                  [(string-contains? (attribute dims-str) "s") #'1]
                  [else #'0])
     #:with unit-name (format-id #'dims #:source #'dims "unit-~a" #'dims)
     #:with go-name (format-id #'dims #:source #'dims "go-~a" #'dims)
     #'(begin (define unit-name (posn col row))
              (define (go-name p) (posn+ p unit-name)))]))

(define-syntax-rule (define-unit-posns dims ...)
  (begin (define-unit-posn dims) ...))

(define unit-o (posn 0 0))
(define-unit-posns n s e w nw ne sw se)

(define (cardinal? x) (memq x '(n s e w)))
(define (ordinal? x) (memq x '(nw ne sw se)))
(define (direction? x) (or (cardinal? x) (ordinal? x)))

(define (get-unit d)
  (match d
    ['o unit-o]
    ['n unit-n]
    ['s unit-s]
    ['e unit-e]
    ['w unit-w]
    ['nw unit-nw]
    ['ne unit-ne]
    ['sw unit-sw]
    ['se unit-se]
    [_ (raise-argument-error 'get-unit "direction?" d)]))
