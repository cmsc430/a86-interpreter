#lang racket

(provide (contract-out
          [int64                  etype?]
          [int32                  etype?]
          [uint64                 etype?]
          [uint32                 etype?]
          ;; TODO: Export more runtime functionality from here? Or elsewhere?
          [current-runtime        (parameter/c runtime?)]
          [step-count             (parameter/c integer?)]
          [current-emulator       (parameter/c emulator?)]
          [emulator-result        (parameter/c a86-value?)]
          [emulator-flag-ref      (case-> (-> flag? boolean?)
                                          (-> emulator? flag? boolean?)
                                          (-> emulator? nonnegative-integer? flag? boolean?))]
          [emulator-register-ref  (case-> (-> register? a86-value?)
                                          (-> emulator? register? a86-value?)
                                          (-> emulator? nonnegative-integer? register? a86-value?))]
          [emulator-memory-ref    (case-> (-> address? a86-value?)
                                          (-> emulator? address? a86-value?)
                                          (-> emulator? nonnegative-integer? address? a86-value?))]
          [emulator-memory-ref/32 (case-> (-> address? a86-value/32-bit?)
                                          (-> emulator? address? a86-value/32-bit?)
                                          (-> emulator? nonnegative-integer? address? a86-value/32-bit?))]
          [convert                (-> a86-value? etype? result/c)]
          [ptr-ref                (->* [a86-value? etype?]
                                       (nonnegative-integer?)
                                       result/c)]
          [asm-emulate            (->* [(listof instruction?)]
                                       [#:after    (-> any/c)
                                        #:on-exit  (-> any/c)
                                        #:on-raise (-> any/c)]
                                       any/c)]
          [asm-emulate/io         (->* [(listof instruction?)
                                        input-port?]
                                       [output-port?
                                        #:after    (-> any/c)
                                        #:on-exit  (-> any/c)
                                        #:on-raise (-> any/c)]
                                       any/c)]))

(require "../ast.rkt"
         "../registers.rkt"
         "../utility.rkt"

         "emulate.rkt"
         "emulator.rkt"
         "etypes.rkt"
         "runtime.rkt"
         "tools.rkt"

         (rename-in "emulator.rkt" [emulator-step-count step-count]))

(define result/c (integer-in (convert min-signed    int64)
                             (convert max-unsigned uint64)))
