#lang racket

(provide current-repl-state
         current-emulator
         current-state-index
         current-emulator-state
         current-instruction-pointer
         current-memory
         current-flags
         current-registers
         current-labels->addresses
         current-flag-ref)

(require "../../exn.rkt"
         "../emulator.rkt"
         "../state.rkt"
         (submod "../emulator.rkt" private))

(struct repl-state
  (emulator confs))

(define current-repl-state (make-parameter #f repl-state?))

(define (current-emulator) (repl-state-emulator (current-repl-state)))

(define (current-state-index)
  (Emulator-current-index (current-emulator)))

(define (current-emulator-state)
  (emulator-state (current-emulator)))

(define (current-instruction-pointer)
  (StepState-ip (current-emulator-state)))

(define (current-memory)
  (Emulator-memory (current-emulator)))

(define (current-flags)
  (emulator->flags (current-emulator)))

(define (current-registers)
  (emulator->registers (current-emulator)))

(define (current-labels->addresses)
  (Emulator-labels->addresses (current-emulator)))

(define (current-flag-ref flag)
  (emulator-flag-ref (current-emulator) flag))

(define (current-register-ref register)
  (emulator-register-ref (current-emulator) register))

(define (current-memory-ref address)
  (emulator-memory-ref (current-emulator) address))

(define (current-transactions)
  (StepState-memory-transactions (current-emulator-state)))

(define (previous-emulator-state)
  (emulator-state (current-emulator) -1))

(define (previous-instruction-pointer)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (StepState-ip (previous-emulator-state))))

(define (previous-flags)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator->flags (current-emulator) -1)))

(define (previous-registers)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator->registers (current-emulator) -1)))

(define (previous-flag-ref flag)
  (with-handlers ([exn:fail:a86? (λ _ 'no-result)])
    (emulator-flag-ref (current-emulator) -1 flag)))

(define (previous-register-ref register)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator-register-ref (current-emulator) -1 register)))

(define (previous-memory-ref address)
  (emulator-memory-ref (current-emulator) -1 address))

(define (previous-transactions)
  (StepState-memory-transactions (previous-emulator-state)))

(define (step-next!)
  (emulator-step! (current-emulator)))

(define (step-prev!)
  (emulator-step-back! (current-emulator)))
