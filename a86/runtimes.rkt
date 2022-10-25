#lang racket

(define runtimes/evildoer
  (hash 'read-byte (λ () (read-byte))
        'peek-byte (λ () (peek-byte))
        'write-byte (λ (b) (write-byte b))))

(define runtimes/extort
  (hash-set* runtimes/evildoer))

(define runtimes/fraud
  (hash-set* runtimes/extort))

(define runtimes/hoax
  (hash-set* runtimes/fraud))

(define runtimes/hoodwink
  (hash-set* runtimes/hoax))

(define runtimes/hustle
  (hash-set* runtimes/hoodwink))

(define runtimes/iniquity
  (hash-set* runtimes/hustle))
