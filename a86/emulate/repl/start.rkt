#lang racket

(require "../emulate.rkt"
         "../exn.rkt"
         "../runtimes.rkt"

         "exn.rkt"
         "repl.rkt"
         "repl-state.rkt"
         "show.rkt")

(module* main #f
  (define input-file    (make-parameter #f))
  (define input-string  (make-parameter #f))
  (define runtime       (make-parameter empty-runtime))
  (define show-mode     (make-parameter 'simple))
  (define catch-errors? (make-parameter #t))
  (command-line
   #:once-each
   [("-f" "--file")    path
                       "The file to read a86 instructions from"
                       (input-file path)]
   [("-i" "--input")   str
                       "The string to use as input"
                       (input-string str)]
   [("-m" "--mode")    mode
                       ;; TODO: These hints/documentation should be attached to
                       ;; the definitions of the modes in [show.rkt].
                       ("The initial 'show' mode; one of:"
                        "  simple   --- the essentials in a single line"
                        "  compact  --- a few lines of useful info"
                        "  complete --- a lot of information")
                       (let ([mode (string->symbol mode)])
                         (unless (assoc mode show-modes)
                           (raise-user-error 'repl "not a valid mode: ~s" mode)))]
   [("-r" "--runtime") rt
                       "The runtime to use"
                       (let ([runtime-name (string->symbol rt)])
                         (unless (runtime-exists? runtime-name)
                           (raise-user-error 'repl "not a valid runtime: ~s" runtime-name))
                         (runtime (name->runtime runtime-name)))]
   [("-E" "--errors")  "Disables error suppression"
                       (catch-errors? #f)]
   #:args ()
   (initialize-repl #:input-file    (input-file)
                    #:input-string  (input-string)
                    #:runtime       (runtime)
                    #:mode          (show-mode)
                    #:catch-errors? (catch-errors?))))

;; TODO: We need I/O versions of these to be useful.
;;
;; TODO: Need an input port that we can track positions in.
;;
;; Turns out there's [file-position] and [file-position*] to get the current
;; "cursor" position within a port, which gives us what we want.
(define (make-repl-emulator-before-thunk show-mode runtime input-port output-port)
  (λ ()
    (current-repl-state (make-repl-state show-mode runtime input-port output-port))
    (current-runtime runtime)
    (show-state)))
(define repl-emulator-after-thunk
  (λ ()
    (when (current-repl-input-port)
      (close-input-port (current-repl-input-port)))
    (when (current-repl-output-port)
      (close-output-port (current-repl-output-port)))
    (current-repl-state #f)))
(define repl-emulator-body-thunk    repl-loop)
(define repl-emulator-exit-handler  default-emulator-exit-handler/io)
(define repl-emulator-raise-handler default-emulator-raise-handler/io)
(define repl-emulator-exn?-handler
  (λ (e)
    (match e
      [(? exn?)
       (displayln (default-emulator-exn?-formatter/io e))
       (raise-a86-emulator-resume-error 'repl-emulator-exn?-handler)])))

(define (initialize-repl #:input-file    [input-file    #f]
                         #:instructions  [instructions  #f]
                         #:input-string  [input-string  #f]
                         #:input-port    [input-port    #f]
                         #:runtime       [runtime       empty-runtime]
                         #:mode          [show-mode     'simple]
                         #:catch-errors? [catch-errors? #t])
  (when (and input-file instructions)
    (raise-a86-user-repl-error 'initialize-repl "given both #:input-file and #:instructions; choose one"))
  (when (not (or input-file instructions))
    (raise-a86-user-repl-error 'initialize-repl "must give either #:input-file or #:instructions"))
  (when (and input-string input-port)
    (raise-a86-user-repl-error 'initialize-repl "given both #:input-string and #:input-port; choose one"))
  (when input-file
    (unless (file-exists? input-file)
      (raise-a86-user-repl-error 'initialize-repl "input file does not exist: ~a" input-file))
    (set! instructions (read-instructions-from-file input-file)))
  (when (and runtime (symbol? runtime))
    (unless (runtime-exists? runtime)
      (raise-a86-user-repl-error 'initialize-repl "no runtime found with name: ~s" runtime))
    (set! runtime (name->runtime runtime)))
  (unless (runtime? runtime)
    (raise-a86-user-repl-error 'initialize-repl "not a runtime: ~v" runtime))
  (unless (symbol? show-mode)
    (raise-a86-user-repl-error 'initialize-repl "show mode must be given as a symbol; got: ~v" show-mode))
  (unless (assoc show-mode show-modes)
    (raise-a86-user-repl-error 'initialize-repl "not a valid show mode: ~s" show-mode))

  (let ([in (or input-port
                (open-input-string (or input-string "")))]
        [out (open-output-string)]
        [show-proc (cadr (assoc show-mode show-modes))])
    (parameterize
        ([repl-catch-errors?             catch-errors?]
         [current-runtime-input-port     in]
         [current-runtime-output-port    out]
         [current-emulator-exit-handler  repl-emulator-exit-handler]
         [current-emulator-exn?-handler  repl-emulator-exn?-handler]
         [current-emulator-raise-handler repl-emulator-raise-handler]
         [current-emulator-before-thunk  (make-repl-emulator-before-thunk show-proc
                                                                          runtime
                                                                          in
                                                                          out)]
         [current-emulator-body-thunk    repl-emulator-body-thunk]
         [current-emulator-after-thunk   repl-emulator-after-thunk])
      (run-emulator instructions in out))))
