#lang racket

(provide/contract
 [current-runtime (parameter/c (hash/c symbol? procedure?))]
 [step-count      (parameter/c integer?)]
 [asm-emulate     (-> (listof instruction?) any/c)]
 #;[asm-emulate/io  (-> (listof instruction?) string? any/c)])

(require "ast.rkt"
         "runtime.rkt"
         "utility.rkt"

         (rename-in "emulator.rkt"
                    [emulator-step-count step-count]))

;; Asm -> Value
;; Interpret (by using an emulator) x86-64 code
;; Assume: entry point is "entry"
(define (asm-emulate instructions)
  (let ([emulator (initialize-emulator instructions)])
    (emulator-multi-step! emulator)
    (a86-value->signed-integer (emulator-register-ref emulator 'rax))))

#;(define (asm-emulate/io instructions input)
  42)
