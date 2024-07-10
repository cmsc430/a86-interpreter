#lang racket

(provide make-repl-state

         current-repl-state
         current-repl-emulator
         current-repl-show-mode
         current-repl-runtime
         current-repl-input-port
         current-repl-output-port
         current-repl-input-port->string
         current-repl-output-port->string
         with-repl-show-mode

         current-repl-emulator-state
         current-repl-emulator-state-index
         current-repl-instruction-pointer
         current-repl-memory
         current-repl-address-readable?
         current-repl-address-writable?
         current-repl-flags
         current-repl-registers
         current-repl-eabels->addresses
         current-repl-flag-ref
         current-repl-register-ref
         current-repl-memory-ref
         current-repl-memory-ref*
         current-repl-instruction
         current-repl-flag-transactions
         current-repl-register-transactions
         current-repl-memory-transactions

         previous-repl-emulator-state
         previous-repl-emulator-state-index
         previous-repl-instruction-pointer
         previous-repl-flags
         previous-repl-registers
         previous-repl-flag-ref
         previous-repl-register-ref
         previous-repl-memory-ref
         previous-repl-memory-ref*
         previous-repl-instruction
         previous-repl-flag-transactions
         previous-repl-register-transactions
         previous-repl-memory-transactions

         repl-next-step!
         repl-prev-step!
         repl-multi-step!)

(require "../../exn.rkt"
         "../emulator.rkt"
         "../memory.rkt"
         "../state.rkt"
         (submod "../emulator.rkt" private)

         (for-syntax syntax/parse))

(struct repl-state
  (emulator
   [show-mode   #:mutable]
   [runtime     #:mutable]
   [input-port  #:mutable]
   [output-port #:mutable]))

(define (make-repl-state emulator
                         [show-mode   'simple]
                         [runtime     #f]
                         [input-port  #f]
                         [output-port #f])
  (repl-state emulator show-mode runtime input-port output-port))

(define current-repl-state (make-parameter #f))

(define (current-repl-emulator)
  (repl-state-emulator (current-repl-state)))

(define (current-repl-show-mode)
  (repl-state-show-mode (current-repl-state)))

(define (current-repl-runtime)
  (repl-state-runtime (current-repl-state)))

(define (current-repl-input-port)
  (repl-state-input-port (current-repl-state)))

(define (current-repl-output-port)
  (repl-state-output-port (current-repl-state)))

(define (current-repl-input-port->string #:from-beginning? [from-beginning? #f])
  (let ([in (current-repl-input-port)])
    (and in
         (input-port? in)
         (let ([fp (file-position* in)]
               [_  (when from-beginning?
                     (file-position in 0))]
               [rs (port->string in #:close? #f)])
           (file-position in fp)
           rs))))

(define (current-repl-output-port->string)
  (let ([out (current-repl-output-port)])
    (and out
         (get-output-string out))))

(define (current-repl-emulator-state)
  (emulator-state (current-repl-emulator)))

(define-syntax (with-repl-show-mode stx)
  (syntax-parse stx
    [(_ new-mode body ...+)
     #'(let ([old-mode (current-repl-show-mode)])
         (dynamic-wind
           (λ () (set-repl-state-show-mode! (current-repl-state) new-mode))
           (λ () body ...)
           (λ () (set-repl-state-show-mode! (current-repl-state) old-mode))))]))

(define (current-repl-emulator-state-index)
  (Emulator-current-index (current-repl-emulator)))

(define (current-repl-instruction-pointer)
  (StepState-ip (current-repl-emulator-state)))

(define (current-repl-memory)
  (Emulator-memory (current-repl-emulator)))

(define (current-repl-address-readable? address)
  (address-readable? (current-repl-memory) address))

(define (current-repl-address-writable? address)
  (address-writable? (current-repl-memory) address))

(define (current-repl-flags)
  (emulator->flags (current-repl-emulator)))

(define (current-repl-registers)
  (emulator->registers (current-repl-emulator)))

(define (current-repl-eabels->addresses)
  (Emulator-labels->addresses (current-repl-emulator)))

(define (current-repl-flag-ref flag)
  (emulator-flag-ref (current-repl-emulator) flag))

(define (current-repl-register-ref register)
  (emulator-register-ref (current-repl-emulator) register))

(define (current-repl-memory-ref address)
  (emulator-memory-ref (current-repl-emulator) address))

(define (current-repl-memory-ref* address n)
  (emulator-memory-ref* (current-repl-emulator) address n))

(define (current-repl-instruction)
  (current-repl-memory-ref (current-repl-instruction-pointer)))

(define (current-repl-flag-transactions)
  (StepState-flag-transactions (current-repl-emulator-state)))

(define (current-repl-register-transactions)
  (StepState-register-transactions (current-repl-emulator-state)))

(define (current-repl-memory-transactions)
  (StepState-memory-transactions (current-repl-emulator-state)))

(define (previous-repl-emulator-state)
  (emulator-state (current-repl-emulator) -1))

(define (previous-repl-emulator-state-index)
  (sub1 (current-repl-emulator-state-index)))

(define (previous-repl-instruction-pointer)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (StepState-ip (previous-repl-emulator-state))))

(define (previous-repl-flags)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator->flags (current-repl-emulator) -1)))

(define (previous-repl-registers)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator->registers (current-repl-emulator) -1)))

(define (previous-repl-flag-ref flag)
  (with-handlers ([exn:fail:a86? (λ _ 'no-result)])
    (emulator-flag-ref (current-repl-emulator) -1 flag)))

(define (previous-repl-register-ref register)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator-register-ref (current-repl-emulator) -1 register)))

(define (previous-repl-memory-ref address)
  (emulator-memory-ref (current-repl-emulator) -1 address))

(define (previous-repl-memory-ref* address n)
  (emulator-memory-ref* (current-repl-emulator) -1 address n))

(define (previous-repl-instruction)
  (previous-repl-memory-ref (previous-repl-instruction-pointer)))

(define (previous-repl-flag-transactions)
  (StepState-flag-transactions (previous-repl-emulator-state)))

(define (previous-repl-register-transactions)
  (StepState-register-transactions (previous-repl-emulator-state)))

(define (previous-repl-memory-transactions)
  (StepState-memory-transactions (previous-repl-emulator-state)))

(define (repl-next-step!)
  (emulator-step! (current-repl-emulator)))

(define (repl-prev-step!)
  (emulator-step-back! (current-repl-emulator)))

(define (repl-multi-step!)
  (emulator-multi-step! (current-repl-emulator)))
