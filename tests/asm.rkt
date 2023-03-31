#lang racket

(provide wrap-asm-test
         run-asm-test)

(require "../a86/ast.rkt"
         "../a86/printer.rkt"
         "../a86/registers.rkt"
         (submod "../a86/printer.rkt" private))

(define THIS-FILE-PATH (syntax-source #'here))
(unless (path? THIS-FILE-PATH)
  (error "not a path:" THIS-FILE-PATH))

(define (dirname p)
  (let-values ([(dir _ __) (split-path p)])
    dir))

(define (basename p)
  (let-values ([(_ base __) (split-path p)])
    base))

(define THIS-DIR (dirname THIS-FILE-PATH))
(define ASM-DIR (build-path THIS-DIR "asm"))

;; Asm symbol? -> Asm
;;
;; NOTE: This code blindly inserts the label ['entry], so the given code cannot
;; use this same label name at any point.
;;
;; TODO: Rename existing ['entry] label if needed.
(define (wrap-asm-test instrs [entry-label #f])
  (if entry-label
      (append (list (Extern 'set_regs)
                    (Extern 'print_results)
                    (Global 'entry)
                    (Label 'entry)
                    (Call entry-label)
                    (Call 'set_regs)
                    (Call 'print_results)
                    (Ret))
              instrs)
      (append (list (Extern 'set_regs)
                    (Extern 'print_results)
                    (Global 'entry)
                    (Label 'entry))
              instrs
              (list (Call 'set_regs)
                    (Call 'print_results)
                    (Ret)))))

;; Asm -> FlagHash RegHash
;;
;; NOTE: Assumes the given instructions have been wrapped properly by the
;; [wrap-asm-test] function.
(define (run-asm-test name
                      instrs
                      #:delete-files [delete-files #t])
  (unless (and (list? instrs) (andmap instruction? instrs))
    (error "expected a list of a86 instructions"))

  (define test.s   (make-temporary-file "test-~a.s" #:base-dir ASM-DIR))
  (define test.run (path-replace-extension test.s #".run"))
  (define test.out (path-replace-extension test.s #".out"))
  (define test.log (path-replace-extension test.s #".log"))

  (with-output-to-file test.s
    #:exists 'replace
    (thunk
     (parameterize ([current-shared? #t])
       (asm-display instrs))))

  (with-output-to-file test.log
    #:exists 'append
    (thunk
     (unless (system (format "make -C ~a ~a" ASM-DIR (basename test.run)))
       (error "make failed"))))

  (with-output-to-file test.out
    (thunk
     (displayln name)))

  (unless (with-output-to-file test.out
            #:exists 'append
            (thunk
             (system (format "~a" test.run))))
    (error "program execution failed"))

  (define-values (flags registers)
    (parse-output (file->lines test.out)))

  (when delete-files
    (delete-file test.s)
    (delete-file test.run)
    (delete-file test.out)
    (delete-file test.log))

  (values flags registers))

(define (prep-line line)
  (match (map string-trim (string-split line "\t"))
    [(cons k vs)
     (cons (string->symbol k)
           vs)]))

(define (parse-flags flags-vs)
  (define flags (map string->symbol (string-split (car flags-vs) ", ")))
  (define flag-hash (make-flags))
  (for ([flag flags])
    (when (flag? flag)
      (set! flag-hash (hash-set flag-hash flag #t))))
  flag-hash)

(define (parse-register reg-vs)
  (string->number (cadr reg-vs)))

(define (parse-output lines)
  (define k:vs
    (make-hasheq (map prep-line
                      (filter (λ (s) (> (string-length (string-trim s)) 0))
                              lines))))
  (values
   (parse-flags (hash-ref k:vs 'flags))
   (for/hash ([reg register-names])
     (values reg (parse-register (hash-ref k:vs reg))))))
