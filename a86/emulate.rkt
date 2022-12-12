#lang racket

(provide/contract
 [current-runtime (parameter/c (hash/c symbol? procedure?))]
 [step-count (parameter/c integer?)]
 [asm-emulate (-> (listof instruction?) any/c)])

(require "ast.rkt"
         "memory.rkt"
         "runtime.rkt"
         "step.rkt"
         "utility.rkt")

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
;; Assume: entry point is "entry"
(define (asm-emulate instructions)
  (let-values ([(text-contents
                 data-contents
                 rodata-contents
                 bss-contents
                 globals  ;; TODO: necessary for something?
                 externs  ;; TODO: necessary for something?
                 label-index-assocs)
                (separate-instructions (sequence (Text)
                                                 (Call (gensym 'entry))
                                                 instructions))])
    (let* ([initial-label (car (car (reverse label-index-assocs)))]
           [text-contents (append (take text-contents (sub1 (length text-contents)))
                                  (list (Call initial-label)))]
           [memory (make-memory #:text-contents text-contents
                                #:data-contents data-contents
                                #:rodata-contents rodata-contents
                                #:bss-contents bss-contents)]
           [instruction-pointer (cdr (section->range memory text))]
           [labels->addresses (compute-label-addresses label-index-assocs
                                                       instruction-pointer)])
      (match (multi-step (initialize-state memory)
                         memory
                         labels->addresses)
        [(cons last-state _)
         (a86-value->signed-integer (hash-ref (state->registers last-state) 'rax))]))))

;; Converts the indices assigned to each label into addresses in the .text
;; section. It is assumed that all indices will properly lie within the bounds
;; of the .text section.
(define (compute-label-addresses label-index-assocs hi-text-address)
  (for/hash ([pair label-index-assocs])
    (values (car pair)
            (word-aligned-offset hi-text-address
                                 (* -1 (cdr pair))))))

;; Separates a flat list of instructions into .text, .data, .rodata, and .bss
;; sections while also extracting global declarations, external declarations,
;; and indexed subroutine labels.
(define (separate-instructions instructions)
  (let separate ([instructions       instructions]
                 [index              0           ]
                 [section            text        ]
                 [text-contents      '()         ]
                 [data-contents      '()         ]
                 [rodata-contents    '()         ]
                 [bss-contents       '()         ]
                 [globals            (set)       ]
                 [externs            (set)       ]
                 [label-index-assocs (list)      ])
    (match instructions
      [(list)
       ;; NOTE: We intentionally do not reverse these lists. This way, the
       ;; contents are written into memory in a top-down manner.
       ;; TODO: Is this the right behavior?
       (values text-contents data-contents rodata-contents bss-contents
               globals externs label-index-assocs)]
      [(cons instruction instructions)
       (let ([separate (λ (#:add-index [add-index   #t]  ;; This one is true!
                           #:section   [new-section #f]  ;; The rest are false.
                           #:text      [new-text    #f]
                           #:data      [new-data    #f]
                           #:rodata    [new-rodata  #f]
                           #:bss       [new-bss     #f]
                           #:globals   [new-globals #f]
                           #:externs   [new-externs #f]
                           #:labels    [new-labels  #f])
                         (separate instructions
                                   (if add-index
                                       (add1 index)
                                       index)
                                   (or new-section section)
                                   (or new-text    text-contents)
                                   (or new-data    data-contents)
                                   (or new-rodata  rodata-contents)
                                   (or new-bss     bss-contents)
                                   (or new-globals globals)
                                   (or new-externs externs)
                                   (or new-labels  label-index-assocs)))])
         (match instruction
           [(Text)     (separate #:add-index #f #:section text)]
           [(Data)     (separate #:add-index #f #:section data)]
           [(Rodata)   (separate #:add-index #f #:section rodata)]
           [(Bss)      (separate #:add-index #f #:section bss)]
           [(Global g) (separate #:add-index #f #:globals (set-add globals g))]
           [(Extern e) (separate #:add-index #f #:externs (set-add externs e))]
           [(Label l)  (separate #:add-index #f #:labels  (cons (cons l index) label-index-assocs))]
           [(or (Dd x) (Dq x))
            (match section
              ['data   (separate #:add-index #f #:data   (cons x data-contents))]
              ['rodata (separate #:add-index #f #:rodata (cons x rodata-contents))]
              ['bss    (separate #:add-index #f #:bss    (cons x bss-contents))])]
           ;; TODO: Comments should probably be saved somewhere for
           ;; reconstruction during debugging.
           [(? Comment?) (separate #:add-index #f)]
           [_ (separate #:text (cons instruction text-contents))]))])))
