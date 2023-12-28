#lang racket

(provide separate-instructions)

(require "../ast.rkt"
         "sections.rkt")

;; Separates a flat list of instructions into .text, .data, .rodata, and .bss
;; sections while also extracting global declarations, external declarations,
;; and indexed subroutine labels.
;;
;; Part of the complexity of this function is due to a choice in not restricting
;; sections to only contiguous instructions.
(define (separate-instructions instructions)
  (let separate ([instructions       instructions]
                 [index              0           ]
                 [section            text        ]
                 [text-contents      '()         ]
                 [data-contents      '()         ]
                 [rodata-contents    '()         ]
                 [bss-contents       '()         ]
                 [labels             (set)       ]
                 [globals            (set)       ]
                 [externs            (set)       ]
                 [label-index-assocs (list)      ])
    (match instructions
      [(list)
       ;; NOTE: We intentionally do not reverse these lists. This way, the
       ;; contents are written into memory in a top-down manner.
       ;; TODO: Is this the right behavior?
       (values text-contents data-contents rodata-contents bss-contents
               labels globals externs label-index-assocs)]
      [(cons instruction instructions)
       (let ([separate (λ (#:add-index    [add-index   #t]  ;; This one is true!
                           #:section      [new-section #f]  ;; The rest are not.
                           #:text         [new-text    #f]
                           #:data         [new-data    #f]
                           #:rodata       [new-rodata  #f]
                           #:bss          [new-bss     #f]
                           #:labels       [new-labels  #f]
                           #:globals      [new-globals #f]
                           #:externs      [new-externs #f]
                           #:label-assocs [new-label-assocs #f])
                         (separate instructions
                                   (if add-index
                                       (add1 index)
                                       index)
                                   (or new-section      section)
                                   (or new-text         text-contents)
                                   (or new-data         data-contents)
                                   (or new-rodata       rodata-contents)
                                   (or new-bss          bss-contents)
                                   (or new-labels       labels)
                                   (or new-globals      globals)
                                   (or new-externs      externs)
                                   (or new-label-assocs label-index-assocs)))])
         (match instruction
           [(Text)     (separate #:add-index #f #:section text)]
           [(Data)     (separate #:add-index #f #:section data)]
           [(Rodata)   (separate #:add-index #f #:section rodata)]
           [(Bss)      (separate #:add-index #f #:section bss)]
           [(Global g) (separate #:add-index #f #:globals (set-add globals g))]
           [(Extern e) (separate #:add-index #f #:externs (set-add externs e))]
           [(Label l)  (separate #:add-index #f
                                 #:labels (set-add labels l)
                                 #:label-assocs (cons (cons l index) label-index-assocs))]
           [(or (Dd x) (Dq x))
            (match section
              ['data   (separate #:add-index #f #:data   (cons x data-contents))]
              ['rodata (separate #:add-index #f #:rodata (cons x rodata-contents))]
              ['bss    (separate #:add-index #f #:bss    (cons x bss-contents))])]
           ;; TODO: Comments should probably be saved somewhere for
           ;; reconstruction during debugging.
           [(? comment?) (separate #:add-index #f)]
           [_ (separate #:text (cons instruction text-contents))]))])))
