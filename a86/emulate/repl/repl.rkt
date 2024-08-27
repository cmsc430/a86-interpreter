#lang racket

(require "../../instructions.rkt"
         "../../registers.rkt"
         "../../utility.rkt"

         "../emulate.rkt"

         "commands.rkt"
         "exn.rkt"
         "format.rkt"
         "parse.rkt"
         "prompt.rkt"
         "repl-state.rkt"
         "show.rkt")

(provide repl-catch-errors?
         repl-loop
         show-state
         quit

         read-instructions-from-file)

(define repl-catch-errors? (make-parameter #t))

(define (repl-loop)
  (with-handlers ([exn? (λ (e)
                          (if (repl-catch-errors?)
                              (begin (displayln (exn-message e))
                                     (repl-loop))
                              (raise e)))])
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
     ((current-repl-show-proc))]
    [else
     (show-dispatch (first xs) (rest xs))]))

(define (show-dispatch x xs)
  (let/ec abort
    (case x
      ;; Display the current instructions.
      [(i is instructions)
       (match xs
         [(list (? exact-integer? n))
          (displayln (format-instructions n))]
         [_
          (displayln (format-instructions 11))])]
      ;; Display part of the stack.
      ;;
      ;; NOTE: We assume [rsp] holds the stack pointer.
      [(s st stack)
       (match xs
         [(list (? positive-integer? n))
          (displayln (format-memory n (current-repl-register-ref 'rsp)))]
         [(list (? number? n))
          (abort (displayln "numerical argument to 'show ~a' must be a positive integer; got ~a" x n))]
         [_
          (displayln (format-memory 4 (current-repl-register-ref 'rsp)))])]
      ;; Display part of memory.
      [(m mem memory)
       (match xs
         [(cons base xs)
          (let ([base-address (cond
                                [(register? base) (current-repl-register-ref base)]
                                [(address?  base) base]
                                [else
                                 (abort (displayln "base argument to 'show ~a' must be a register or address; got ~a"
                                                   x
                                                   base))])])
            (match xs
              ['()
               (displayln (format-memory 4 base-address #:align 'top))]
              [(list (and (? exact-integer? n)
                          (not (? zero?))))
               (displayln (format-memory (abs n)
                                         base-address
                                         #:align (if (negative-integer? n)
                                                     'top
                                                     'bottom)))]
              [(list (? number? n))
               (abort (displayln "numerical argument to 'show ~a ~a' must be a positive or negative integer; got ~a"
                                 x
                                 base
                                 n))]
              [v
               (abort (displayln "unknown 'show ~a ~a' argument: ~a" x base v))]))])]
      ;; Display flag(s).
      [(f fs flag flags)
       (displayln
        (string-join
         (map (compose1
               (λ (f) (format/repl "~a: ~f!" f f))
               (λ (f)
                 (if (flag? f)
                     f
                     (abort (displayln (format "unknown flag in 'show ~a': ~a" x f)))))
               string->symbol
               (λ (s)
                 (match (string-length s)
                   [1 (string-append s "F")]
                   [2 s]
                   [_ (abort (displayln (format "unknown flag in 'show ~a': ~a" x s)))]))
               string-upcase
               symbol->string)
              xs)
         "\n"))]
      ;; Display register(s).
      [(r rs register registers)
       (displayln
        (string-join
         (map (compose1
               ;; TODO: debug why ~r is also printing the name of the register.
               ;; I think [expand]ing the [define-format-for-show] macro used
               ;; for [format/repl] will help here
               (λ (r) (format/repl "~a: ~r" r r))
               (λ (r)
                 (if (register? r)
                     r
                     (abort (displayln (format "unknown register in 'show ~a': ~a" x r))))))
              xs)
         "\n"))]
      ;; Attempt to display the arguments.
      [else
       (for ([x (in-list xs)])
         (match x
           [(? flag?)
            (displayln (format/repl "~a: ~f!" x x))]
           [(? register?)
            (displayln (format/repl "~a: ~r" x x))]
           [_
            (abort (displayln (format "unknown 'show' argument: ~a" x)))]))])))

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

;; TODO: Move elsewhere.
(define (read-instructions-from-file path-str)
  (with-input-from-file (cleanse-path (string->path path-str))
    (λ () (read-instructions))))

;; TODO: Move elsewhere.
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
