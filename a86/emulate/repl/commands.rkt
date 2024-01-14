#lang racket

(provide (struct-out command)
         define-commands)

(require (for-syntax racket/string
                     racket/syntax
                     syntax/parse))

(struct command (name aliases invoke-proc short-help))

(define-syntax (define-commands stx)
  (syntax-parse stx
    [(_ root-name:id
        (((~seq (~or (~and root-command-name:id (~bind [(alias-command-names 1) (list)]))
                     (root-command-name:id alias-command-names ...))
                proc:id
                (~or (~and help-str:str (~bind [(fmt-args 1) (list)]))
                     (~and (help-strs:str ...+ fmt-args ...)
                           (~bind [help-str #`#,(string-append* (map syntax-e (syntax->list #'(help-strs ...))))])))))
         ...))
     #:with invoke-name      (format-id #'root-name "~a-invoke"           #'root-name)
     #:with name->short-help (format-id #'root-name "~a-name->short-help" #'root-name)
     #:with name->command    (format-id #'root-name "~a-name->command"    #'root-name)
     #:with (command-struct-name ...) (map (λ (name-stx) (format-id name-stx "~a-command" name-stx))
                                           (syntax->list #'(root-command-name ...)))
     #:with (formatted-help-str ...) (map (λ (help-str-stx fmt-args-stxs)
                                            (if (null? (syntax->list fmt-args-stxs))
                                                help-str-stx
                                                (with-syntax ([help-str help-str-stx]
                                                              [(fmt-args ...) fmt-args-stxs])
                                                  #'(format help-str fmt-args ...))))
                                          (syntax->list #'(help-str ...))
                                          (syntax->list #'((fmt-args ...) ...)))
     #'(begin
         (define command-struct-name
           (command 'root-command-name
                    (list 'alias-command-names ...)
                    proc
                    formatted-help-str)) ...
         (define root-name (list command-struct-name ...))
         (define (invoke-name name . args)
           (match name
             [(or 'root-command-name 'alias-command-names ...) (apply proc args)] ...
             [_ (raise-user-error 'invoke-name "Unknown command name: ~a" name)]))
         (define (name->short-help name)
           (match name
             [(or 'root-command-name 'alias-command-names ...) (command-short-help command-struct-name)] ...
             [_ (raise-user-error 'name->short-help "Unknown command name: ~a" name)]))
         (define (name->command name)
           (match name
             [(or 'root-command-name 'alias-command-names ...) command-struct-name] ...
             [_ (raise-user-error 'name->command "Unknown command name: ~a" name)])))]))
