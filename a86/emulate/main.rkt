#lang racket

(provide (contract-out                  ;; TODO: Rearrange these.
          [int64            etype?]
          [int32            etype?]
          [uint64           etype?]
          [uint32           etype?]
          ;; TODO: Export more runtime functionality from here? Or elsewhere?
          [current-runtime  (parameter/c runtime?)]
          [step-count       (parameter/c integer?)]
          [current-emulator (parameter/c emulator?)]
          [emulator-result  (parameter/c a86-value?)]
          [convert          (-> a86-value? etype? result/c)]
          [ptr-ref          (->* (a86-value? etype?)
                                 (nonnegative-integer?)
                                 result/c)]
          [asm-emulate      (->* ([listof instruction?])
                                 (#:after (-> any/c)
                                  #:on-error (-> any/c))
                                 any/c)]
          #;[asm-emulate/io  (-> (listof instruction?) string? any/c)]))

(require "../ast.rkt"
         "../utility.rkt"

         "emulate.rkt"
         "emulator.rkt"
         "etypes.rkt"
         "runtime.rkt"
         "tools.rkt"

         (rename-in "emulator.rkt" [emulator-step-count step-count]))

(define result/c (integer-in (convert min-signed    int64)
                             (convert max-unsigned uint64)))