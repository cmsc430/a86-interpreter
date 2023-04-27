#lang racket

(provide (struct-out Program)
         make-program
         make-memory-from-program
         compute-label-addresses)

(require "../ast.rkt"
         "../utility.rkt"

         "memory.rkt"
         "separation.rkt")

(struct Program (text-contents
                 data-contents
                 rodata-contents
                 bss-contents
                 label-set
                 label-index-assocs)
  #:transparent)

(define (make-program instructions)
  (let-values ([(text-contents
                 data-contents
                 rodata-contents
                 bss-contents
                 labels
                 globals  ;; TODO: is this necessary?
                 externs  ;; TODO: is this necessary?
                 label-index-assocs)
                (separate-instructions
                 (seq (Text)
                      (Call (gensym 'dummy_entry))
                      (Ret)
                      instructions))])
    (let* ([initial-label (car (car (reverse label-index-assocs)))]
           ;; Drop the call to the dummy initial label and replace it with a
           ;; call to the actual initial label.
           [text-contents (append (take text-contents (sub1 (length text-contents)))
                                  (list (Call initial-label)))])
      (Program text-contents data-contents rodata-contents bss-contents labels label-index-assocs))))

;; Initializes memory from a [Program?].
(define (make-memory-from-program program)
  (match program
    [(Program tc dc rc bc _ _)
     (make-memory #:text-contents   tc
                  #:rodata-contents rc
                  #:data-contents   dc
                  #:bss-contents    bc)]))

;; Converts the indices assigned to each label into addresses in the .text
;; section. It is assumed that all indices will properly lie within the bounds
;; of the .text section.
(define (compute-label-addresses program hi-text-address)
  (let ([li-assocs (Program-label-index-assocs program)])
    (for/hash ([pair li-assocs])
      (values (car pair)
              (word-aligned-offset hi-text-address
                                   (* -1 (cdr pair)))))))
