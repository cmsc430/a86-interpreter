#lang racket

(provide read-transaction?
         write-transaction?
         transaction-destination
         transaction-value

         ;; Fundamental field accessors.
         read-destination
         read-value
         written-destination
         written-value

         ;; Read transaction creation.
         make-flag-read-transaction
         make-register-read-transaction
         make-memory-read-transaction
         ;; Write transaction creation.
         make-flag-write-transaction
         make-register-write-transaction
         make-memory-write-transaction

         ;; Read transaction predicates.
         flag-read-transaction?
         register-read-transaction?
         memory-read-transaction?
         ;; Write transaction predicates.
         flag-write-transaction?
         register-write-transaction?
         memory-write-transaction?

         ;; Extra field accessors.
         read-byte-count
         written-byte-count

         ;; StepState struct.
         (struct-out StepState))

(require (for-syntax (submod "../utility.rkt" racket/syntax)
                     racket/syntax
                     syntax/parse))


(struct Transaction (read? destination value) #:transparent)
(define read-transaction? Transaction-read?)
(define (write-transaction? t) (not (read-transaction? t)))
(define transaction-destination Transaction-destination)
(define transaction-value Transaction-value)

(define (read-destination    t) (and ( read-transaction? t) (Transaction-destination t)))
(define (read-value          t) (and ( read-transaction? t) (Transaction-value       t)))
(define (written-destination t) (and (write-transaction? t) (Transaction-destination t)))
(define (written-value       t) (and (write-transaction? t) (Transaction-value       t)))

(define-syntax (define-transactions stx)
  (syntax-parse stx
    [(_ [(Name:id extra-field:id ...) ...])
     (let* ([Names (syntax->list #'(Name ...))]
            [names (format-ids name-stx
                               Names
                               "~a"
                               (datum->syntax name-stx
                                              (string->symbol
                                               (string-downcase
                                                (symbol->string
                                                 (syntax-e name-stx))))
                                              name-stx))]
            [extra-fields (map syntax->list (syntax->list #'((extra-field ...) ...)))])
       (with-syntax*
         ([(StructName               ...) (format-ids Names "~aTransaction")]
          [(StructName?              ...) (format-ids (syntax->list #'(StructName ...)) "~a?")]
          [(make-read-name           ...) (format-ids names "make-~a-read-transaction")]
          [(make-write-name          ...) (format-ids names "make-~a-write-transaction")]
          [(read-name?               ...) (format-ids names "~a-read-transaction?")]
          [(write-name?              ...) (format-ids names "~a-write-transaction?")]
          [((StructName-field   ...) ...) (map (λ (StructName-lctx field-lctxs)
                                                 (format-ids field-lctx field-lctxs "~a-~a" StructName-lctx field-lctx))
                                               (syntax->list #'(StructName ...)) extra-fields)]
          [((read-field-name    ...) ...) (map (λ (field-lctxs)
                                                 (format-ids field-lctxs "read-~a"))
                                               extra-fields)]
          [((written-field-name ...) ...) (map (λ (field-lctxs)
                                                 (format-ids field-lctxs "written-~a"))
                                               extra-fields)])
         (quasisyntax/loc stx
           (begin
             (struct StructName Transaction (extra-field ...) #:transparent) ...
             (define (make-read-name destination value extra-field ...)
               (StructName #t destination value extra-field ...)) ...
             (define (make-write-name destination value extra-field ...)
               (StructName #f destination value extra-field ...)) ...
             (define (read-name?  t) (and (StructName? t) (read-transaction?  t))) ...
             (define (write-name? t) (and (StructName? t) (write-transaction? t))) ...
             (begin
               (define (read-field-name t)
                 (and (read-transaction? t)
                      (StructName-field t))) ...
               (define (written-field-name t)
                 (and (write-transaction? t)
                      (StructName-field t))) ...) ...))))]))

(define-transactions
  [(Flag)
   (Register)
   (Memory byte-count)])

;; The current state of the interpreter.
(struct StepState
  (time-tick                            ;; nonnegative-integer?
   ip                                   ;; address?
   flags                                ;; flags?
   registers                            ;; registers?
   flag-transactions                    ;; [Listof FlagTransaction?]
   register-transactions                ;; [Listof RegisterTransaction?]
   memory-transactions)                 ;; [Listof MemoryTransaction?]
  #:transparent)
