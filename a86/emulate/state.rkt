#lang racket

(provide make-read-transaction
         make-write-transaction
         read-transaction?
         write-transaction?
         transaction-address
         transaction-value
         transaction-byte-count

         read-address
         read-value
         read-byte-count
         written-address
         written-value
         written-byte-count

         (struct-out StepState))

(struct Transaction (type address value byte-count) #:transparent)

(define (make-read-transaction address value byte-count)
  (Transaction 'read address value byte-count))
(define (make-write-transaction address value byte-count)
  (Transaction 'write address value byte-count))
(define (read-transaction?  t) (eq? 'read  (Transaction-type t)))
(define (write-transaction? t) (eq? 'write (Transaction-type t)))
(define transaction-address    Transaction-address)
(define transaction-value      Transaction-value)
(define transaction-byte-count Transaction-byte-count)

(define (read-address       t) (and (read-transaction?  t) (Transaction-address t)))
(define (read-value         t) (and (read-transaction?  t) (Transaction-value t)))
(define (read-byte-count    t) (and (read-transaction?  t) (Transaction-byte-count t)))
(define (written-address    t) (and (write-transaction? t) (Transaction-address t)))
(define (written-value      t) (and (write-transaction? t) (Transaction-value t)))
(define (written-byte-count t) (and (write-transaction? t) (Transaction-byte-count t)))

;; The current state of the interpreter.
(struct StepState (time-tick ip flags registers memory-transactions) #:transparent)
