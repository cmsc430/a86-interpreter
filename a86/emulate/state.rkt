#lang racket

(provide read-transaction?
         write-transaction?

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

(require (for-syntax racket/syntax
                     syntax/parse))


(struct Transaction (read? destination value) #:transparent)
(define read-transaction? Transaction-read?)
(define (write-transaction? t) (not (read-transaction? t)))

(define (read-destination    t) (and ( read-transaction? t) (Transaction-destination t)))
(define (read-value          t) (and ( read-transaction? t) (Transaction-value       t)))
(define (written-destination t) (and (write-transaction? t) (Transaction-destination t)))
(define (written-value       t) (and (write-transaction? t) (Transaction-value       t)))

(define-syntax (define-transactions stx)

  ;; Like [format-id], but for lists of identifiers. The [v-procs] can each be
  ;; one of three things:
  ;;
  ;;   1. A single-argument function that takes the current [syntax?] being
  ;;      formatted and returns a value that would be accepted by [format-id]'s
  ;;      format string.
  ;;   2. The value [#f], which will be substituted with the current [syntax?]
  ;;      being formatted (as though the identity function had been passed in).
  ;;   3. Any other value, in which case the value is preserved as-is.
  ;;
  ;;   lctxs   : [Listof syntax?]
  ;;   fmt     : string?
  ;;   v-procs : [Listof (or/c (-> syntax? any/c?)
  ;;                           #f
  ;;                           any/c)]
  (define (format-ids lctxs fmt . v-procs)
    (map (λ (lctx)
           (apply format-id
                  lctx
                  fmt
                  (map (λ (f)
                         (cond [(procedure? f) (f lctx)]
                               [(eq? f #f)     lctx]
                               [else           f]))
                       v-procs)))
         lctxs))

  (syntax-parse stx
    [(_ [(Name:id extra-field:id ...) ...])
     (let* ([Names (syntax->list #'(Name ...))]
            [names (format-ids Names
                               "~a"
                               (λ (name-stx) (datum->syntax name-stx
                                                            (string->symbol
                                                             (string-downcase
                                                              (symbol->string
                                                               (syntax-e name-stx))))
                                                            name-stx)))]
            [extra-fields (map syntax->list (syntax->list #'((extra-field ...) ...)))])
       (with-syntax*
         ([(StructName               ...) (format-ids Names "~aTransaction" #f)]
          [(StructName?              ...) (format-ids (syntax->list #'(StructName ...)) "~a?" #f)]
          [(make-read-name           ...) (format-ids names "make-~a-read-transaction" #f)]
          [(make-write-name          ...) (format-ids names "make-~a-write-transaction" #f)]
          [(read-name?               ...) (format-ids names "~a-read-transaction?" #f)]
          [(write-name?              ...) (format-ids names "~a-write-transaction?" #f)]
          [((StructName-field   ...) ...) (map (λ (StructName-lctx field-lctxs)
                                                 (format-ids field-lctxs "~a-~a" StructName-lctx #f))
                                               (syntax->list #'(StructName ...)) extra-fields)]
          [((read-field-name    ...) ...) (map (λ (field-lctxs)
                                                 (format-ids field-lctxs "read-~a" #f))
                                               extra-fields)]
          [((written-field-name ...) ...) (map (λ (field-lctxs)
                                                 (format-ids field-lctxs "written-~a" #f))
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
