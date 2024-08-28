#lang racket

(provide (contract-out
          [char                            etype?]
          [uchar                           etype?]
          [int64                           etype?]
          [int32                           etype?]
          [uint64                          etype?]
          [uint32                          etype?]
          [current-emulator-flag-ref       (-> flag? boolean?)]
          [current-emulator-register-ref   (-> register? a86-value?)]
          [current-emulator-memory-ref     (-> address? any/c)]
          [current-emulator-memory-ref/32  (-> address? any/c)]
          [previous-emulator-flag-ref      (-> flag? boolean?)]
          [previous-emulator-register-ref  (-> register? a86-value?)]
          [previous-emulator-memory-ref    (-> address? any/c)]
          [previous-emulator-memory-ref/32 (-> address? any/c)]
          [convert                         (-> integer? etype? result/c)]
          [ptr-ref                         (->* [a86-value? etype?]
                                                (nonnegative-integer?)
                                                result/c)]
          [step-count                      (parameter/c integer?)]
          [current-emulator                (parameter/c emulator?)]
          [persist-current-emulator?       (parameter/c boolean?)]
          [run-emulator                    (-> (listof instruction?) input-port? output-port? any/c)]
          [asm-emulate                     (->* [(or/c #f (listof instruction?))]
                                                [#:before   (-> any/c)
                                                 #:during   (-> any/c)
                                                 #:after    (-> any/c)
                                                 #:on-exit  (-> any/c any/c)
                                                 #:on-exn   (-> exn?  any/c)
                                                 #:on-raise (-> any/c any/c)]
                                                any/c)]
          [asm-emulate/io                  (->* [(or/c #f (listof instruction?))
                                                 input-port?]
                                                [output-port?
                                                 #:before   (-> any/c)
                                                 #:during   (-> any/c)
                                                 #:after    (-> any/c)
                                                 #:on-exit  (-> any/c any/c)
                                                 #:on-exn   (-> exn?  any/c)
                                                 #:on-raise (-> any/c any/c)]
                                                any/c)]))

(require "../ast.rkt"
         "../registers.rkt"
         "../utility.rkt"

         "emulate.rkt"
         "emulator.rkt"
         "type-tools.rkt"

         (rename-in "emulator.rkt" [emulator-step-count step-count]))

(define result/c (integer-in (convert min-signed    int64)
                             (convert max-unsigned uint64)))
