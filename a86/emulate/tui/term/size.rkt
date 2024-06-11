#lang racket

(provide (struct-out size)

         size=?
         size<?
         size<=?
         size>?
         size>=?)

(require (for-syntax racket/list
                     syntax/parse))

(struct size (w h) #:transparent)

(define-syntax (define-size-op-func stx)
  (syntax-parse stx
    [(_ name:id)
     #:attr name-parts (regexp-match
                        #px"^size(.+?)[?]$"
                        (symbol->string (syntax-e #'name)))
     #:fail-when (and (not (attribute name-parts)) #'name) "invalid size-op-func name"
     #:attr op-str (second (attribute name-parts))
     #:with op (datum->syntax #'name (string->symbol (attribute op-str)))
     #'(define name
         (match-lambda** [((size w1 h1) (size w2 h2))
                          (and (op w1 w2) (op h1 h2))]))]))

(define-syntax-rule (define-size-op-funcs [name ...])
  (begin (define-size-op-func name) ...))

(define-size-op-funcs
  [size=? size<? size<=? size>? size>=?])
