;; This implementation of key-reading is inspired by
;; [charterm](https://docs.racket-lang.org/charterm/index.html), but has been
;; simplified to suit the lesser needs of this project.
#lang racket

(provide current-tty-path
         current-tty-input-port
         current-tty-output-port
         with-tty
         stty*
         stty*/capture)

(require (for-syntax syntax/parse))

(define stty-path (find-executable-path "stty"))

(define stty-f-arg-string
  (case (system-type 'os*)
    [(macosx freebsd netbsd openbsd) "-f"]
    [else                            "-F"]))

(define default-tty-path (path->string (cleanse-path "/dev/tty")))

(define current-tty (make-parameter #f))
(define (current-tty-path)        (and (current-tty) (tty-path        (current-tty))))
(define (current-tty-input-port)  (and (current-tty) (tty-input-port  (current-tty))))
(define (current-tty-output-port) (and (current-tty) (tty-output-port (current-tty))))

(struct tty (path
             [input-port  #:mutable]
             [output-port #:mutable]))

(define (open-tty [path default-tty-path])
  (with-handlers ([(λ _ #t)
                   (λ (e)
                     (error 'open-tty "failed to open tty ~a\n exception: ~a" path e))])
    (let*-values ([(in out) (open-input-output-file path #:exists 'update)]
                  [(tty)    (tty path in out)])
      (unless (stty* #:tty tty "raw" "-echo")
        (error 'open-tty "could not initialize raw input mode"))
      tty)))

(define (close-tty [tty (current-tty)])
  (with-handlers ([(λ _ #t) (void)]) (close-input-port  (tty-input-port  tty)))
  (with-handlers ([(λ _ #t) (void)]) (close-output-port (tty-output-port tty)))
  (if (with-handlers ([(λ _ #t) (λ _ #f)])
        (stty* #:tty tty "cooked" "echo"))
      (if (eq? tty (current-tty))
          (current-tty #f)
          (void))
      (error 'close-tty "failed to close tty ~a" (tty-path tty))))

(define-syntax (with-tty stx)
  (syntax-parse stx
    [(_ (~seq #:path path) body ...+)
     #'(let ([new-tty #f])
         (dynamic-wind
           (λ ()
             (set! new-tty (open-tty path)))
           (λ ()
             (parameterize ([current-tty new-tty])
               body ...))
           (λ ()
             (close-tty new-tty)
             (set! new-tty #f))))]
    [(_ body ...+)
     #'(with-tty #:path default-tty-path body ...)]))

(define (stty* #:tty [tty (current-tty)] . args)
  (apply system*
         stty-path
         stty-f-arg-string
         (tty-path tty)
         args))

(define (stty*/capture #:tty [tty (current-tty)] . args)
  (parameterize ([current-output-port (open-output-bytes)]
                 [current-input-port  (open-input-bytes #"")]
                 [current-error-port  (open-output-bytes)])
    (unless (apply stty* #:tty tty args)
      (error 'stty*/capture
             "stty process failed\n captured output:\n~a\ncaptured error output:\n~a"
             (get-output-bytes (current-output-port))
             (get-output-bytes (current-error-port))))
    (get-output-bytes (current-output-port))))
