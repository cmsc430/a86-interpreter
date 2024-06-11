#lang racket

(require "../../instructions.rkt"
         "../../registers.rkt"

         "../emulate.rkt"
         "../runtime.rkt"

         "commands.rkt"
         "exn.rkt"
         "format.rkt"
         "parse.rkt"
         "prompt.rkt"
         "repl-state.rkt"
         "show.rkt"

         racket/cmdline)

;; FIXME: update
(provide (all-defined-out))

(module* main #f
  (define input-file   (make-parameter #f))
  (define input-string (make-parameter #f))
  (define runtime      (make-parameter #f))
  (define show-mode    (make-parameter 'simple))
  (command-line
   #:once-each
   [("-f" "--file")    path
                       "The file to read a86 instructions from"
                       (input-file path)]
   [("-i" "--input")   str
                       "The string to use as input"
                       (input-string str)]
   [("-m" "--mode")    mode
                       ("The initial 'show' mode; one of:"
                        "  simple  --- the essentials in a single line"
                        "  compact --- a few lines of useful info")
                       (let ([mode (string->symbol mode)])
                         (case mode
                           [(simple compact)
                            (show-mode mode)]
                           [else (raise-user-error "not a valid mode: ~a" mode)]))]
   [("-r" "--runtime") rt
                       "The runtime to use"
                       (runtime (name->runtime (string->symbol rt)))]
   #:args ()
   (initialize-repl (input-file)
                    (input-string)
                    (runtime)
                    (show-mode))))

(define (repl-loop)
  (with-handlers ([exn? (λ (e)
                          (displayln (exn-message e))
                          (repl-loop))])
    (match (parse (prompt))
      [(cons command args)
       (with-handlers ([exn:fail:a86:user:repl:bad-command?
                        (λ (e)
                          (displayln (format "command not supported: ~a" (exn:fail:a86:user:repl:bad-command-name e))))])
         (apply commands-invoke command args))
       (repl-loop)]
      ['()
       (repl-loop)]
      [#f
       (displayln "")])))

(define (display-help [command-name #f])
  (if command-name
      (displayln (commands-name->short-help command-name))
      (displayln "general help not yet available")))

(define (show-state . xs)
  (cond
    [(not (current-repl-emulator))
     (displayln "Emulator not currently running.")]
    [(empty? xs)
     (case (current-repl-show-mode)
       [(simple)
        (show/simple)]
       [(compact)
        (show/compact)]
       [(#f)
        (displayln "no 'show' mode set")]
       [else
        (displayln (format "unknown 'show' mode: ~a" (current-repl-show-mode)))])]
    [else
     (let/ec abort
        (match (first xs)
          [(or 'f 'fs 'flag 'flags)
           (displayln (string-join
                       (map (compose1
                             (λ (f) (format/repl "~a: ~f!" f f))
                             (λ (f)
                               (if (flag? f)
                                   f
                                   (abort (displayln (format "unknown flag in 'show ~a': ~a" (first xs) f)))))
                             string->symbol
                             (λ (s)
                               (match (string-length s)
                                 [1 (string-append s "F")]
                                 [2 s]
                                 [_ (abort (displayln (format "unknown flag in 'show ~a': ~a" (first xs) s)))]))
                             string-upcase
                             symbol->string)
                            (rest xs))
                       "\n"))]
          [(or 'r 'rs 'reg 'regs 'register 'registers)
           (displayln (string-join
                       (map (compose1
                             ;; TODO: debug why ~r is also printing the name of the register.
                             ;; I think [expand]ing the [define-format-for-show] macro used
                             ;; for [format/repl] will help here
                             (λ (r) (format/repl "~a: ~r foo" r r))
                             (λ (r)
                               (if (register? r)
                                   r
                                   (abort (displayln (format "unknown register in 'show ~a': ~a" (first xs) r))))))
                            (rest xs))
                       "\n"))]
          [_
           (for ([x (in-list xs)])
             (match x
               [(? flag?)
                (displayln (format/repl "~a: ~f!" x x))]
               [(? register?)
                (displayln (format/repl "~a: ~r" x x))]
               [_
                (abort (displayln (format "unknown 'show' argument: ~a" x)))]))]))]))

(define (step-next [steps 1])
  (for ([_ (in-range steps)])
    (repl-next-step!)
    (show-state)))

(define (step-prev [steps 1])
  (for ([_ (in-range steps)])
    (repl-prev-step!)
    (show-state)))

(define (run-to-end)
  (let recurse ([prev-state (current-repl-emulator-state)])
    (repl-next-step!)
    (let ([curr-state (current-repl-emulator-state)])
      (with-repl-show-mode 'simple (show-state))
      (if (eq? prev-state curr-state)
          (with-repl-show-mode 'compact
            (displayln "")
            (show-state))
          (recurse curr-state)))))

(define (quit) #f)

(define (read-instructions-from-file path-str)
  (with-input-from-file (cleanse-path (string->path path-str))
    (λ () (read-instructions))))

(define (load-program path)
  (let ([instructions (read-instructions-from-file (symbol->string path))])
    (reload-emulator instructions #f #f)))

(define-commands commands
  ([( next  n)    step-next    "Execute the next instruction."]
   [( prev  p)    step-prev    "Restore the state of the previous instruction execution."]
   [( run   r)    run-to-end   "Take steps until the program halts or an exception occurs."]
   [( show  s)    show-state   "Display information."]
   [(:quit :q)    quit         "Exit the REPL."]
   [(:help :h :?) display-help "Displays helpful information (allegedly)."]
   [(:load :l)    load-program "Loads an a86 program from a given file path."]))

;; TODO: We need I/O versions of these to be useful.
;;
;; TODO: Need an input port that we can track positions in.
;;
;; Turns out there's [file-position] and [file-position*] to get the current
;; "cursor" position within a port, which gives us what we want.
(define (make-repl-emulator-before-thunk show-mode runtime input-port output-port)
  (λ ()
    (current-repl-state (make-repl-state (current-emulator) show-mode runtime input-port output-port))
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
(define repl-emulator-exn?-handler  default-emulator-exn?-handler/io)
(define repl-emulator-raise-handler default-emulator-raise-handler/io)

(define (initialize-repl [input-file   #f]
                         [input-string #f]
                         [runtime      #f]
                         [show-mode    'simple])
  (let ([instructions (and input-file
                           (read-instructions-from-file input-file))]
        [in (and input-string
                 (open-input-string input-string))]
        [out (open-output-string)])
    (parameterize
        ([current-runtime-input-port     in]
         [current-runtime-output-port    out]
         [current-emulator-exit-handler  repl-emulator-exit-handler]
         [current-emulator-exn?-handler  repl-emulator-exn?-handler]
         [current-emulator-raise-handler repl-emulator-raise-handler]
         [current-emulator-before-thunk  (make-repl-emulator-before-thunk show-mode
                                                                          runtime
                                                                          in
                                                                          out)]
         [current-emulator-body-thunk    repl-emulator-body-thunk]
         [current-emulator-after-thunk   repl-emulator-after-thunk])
      (run-emulator instructions in out))))
