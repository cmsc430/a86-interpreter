#lang racket

(provide make-frameset
         frameset-select-frame!
         frameset-add-frame!)

(require "frame.rkt")

;; A frameset doesn't do much. Its purpose is to hold all the frames and select
;; among them.
(struct frameset (frames [current-frame-id #:mutable]) #:transparent)

(define (make-frameset f-id term-size)
  (frameset (make-hash f-id (make-frame term-size)) f-id))

(define frameset-current-frame
  (match-lambda
    [(frameset fs curr-id) (hash-ref fs curr-id)]))

(define (frameset-frame-id? fs f-id)
  (hash-has-key? (frameset-frames fs) f-id))

(define (frameset-select-frame! fs f-id)
  (unless (frameset-frame-id? fs f-id)
    (raise-argument-error 'frameset-select-frame! "frameset-frame-id?" f-id))
  (set-frameset-current-frame-id! fs f-id)
  (let ([f (frameset-current-frame fs)])
    (frame-refresh! f)
    f))

(define (frameset-add-frame! fs f-id term-size)
  (when (frameset-frame-id? f-id)
    (raise-arguments-error 'frameset-add-frame!
                           "frame name already exists in frameset"
                           "name" f-id
                           "frameset" fs))
  (let ([f (make-frame term-size)])
    (hash-set! (frameset-frames fs) f-id f)
    (frame-initialize! f)
    f))
