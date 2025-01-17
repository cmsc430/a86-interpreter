#lang racket

(provide should-stop?
         exn?-handler
         raise-handler
         (loop-command-out step exit handle-error switch)

         run-emulator
         asm-emulate
         asm-emulate/io)

(require "../utility.rkt"

         "command-loop.rkt"
         "emulator.rkt"
         "runtime.rkt"
         "stacktrace.rkt"

         (submod "emulator.rkt" private))

;; A parameterized nullary function that determines whether execution should
;; halt at this point.
;;
;; NOTE: A tricky thing about this is we don't want to preemptively execute
;; extra steps in the emulator just to discern whether *now* is the right time
;; to stop. Instead, we will have to retroactively check whether we /should
;; have/ stopped prior to the most recent step.
(define should-stop?
  (make-parameter
   (λ ()
     (Emulator-terminated? (current-context-emulator)))))

;; Takes a step within the current emulator loop.
(define-loop-command step #:default
  #:main
  ;; If an error occurs while stepping, capture it and pass it to the customized
  ;; [handle-error] command.
  [(with-handlers ([(λ _ #t)
                    (λ (e) `(handle-error ,e))])
     (current-emulator-step!))]
  #:post
  [(match (main-result)
     ;; If [current-emulator-step!] exited normally, check if we should proceed.
     [(? void?)
      (if ((should-stop?))
          '(exit #t)
          'step)]
     ;; Otherwise, we should just propagate the [main-result].
     [v v])])

;; Terminates the emulator loop.
;;
;; If [graceful?] is true, returns the signed integer representation of the
;; contents of [rax]. If output was captured during execution, the result is
;; returned as a pair instead with the integer representation in the [car]
;; position and the output port (or the contents of that port if it is a
;; [string-port?]) in the [cdr] position.
;;
;; If [graceful?] is [#f], an error is raised using the value of [v].
(define-loop-command (exit graceful? [v #f])
  #:main
  [(if graceful?
       (let* ([r (current-emulator-register-ref 'rax)]
              [r (if (a86-value? r)
                     (a86-value->signed-integer r)
                     r)]
              [p (current-context-output-port)])
         ;; Exit the command loop, returning the result value.
         (abort-command-loop
          (if p
              (cons r
                    (if (string-port? p)
                        (get-output-string p)
                        p))
              r)))
       (raise-user-error "program exited with status ~a" v))])

;; A parameter that holds a procedure to be used when an exception is passed to
;; [handle-error]. The procedure must accept three arguments: an [exn?] and an
;; output port and input port. Note that the output/input ports can be given
;; [#f] (when I/O is not configured in the emulator).
(define exn?-handler
  (make-parameter
   (λ (e output-port input-port)
     (raise-user-error
      (if output-port
          ;; I/O is configured.
          (format "error encountered during I/O evaluation:\n\n  ~a\n\n  machine stack trace:\n~a\n~a\n~a"
                       (string-join (for/list ([str (in-list (string-split (exn-message e)
                                                                           "\n"))])
                                      (if (non-empty-string? str)
                                          (string-append "  " str)
                                          str))
                                    "\n")
                       (string-join (for/list ([str (in-list (string-split (format-stacktrace (current-context-emulator))
                                                                           "\n"))])
                                      (if (non-empty-string? str)
                                          (string-append "  " str)
                                          str))
                                    "\n")
                       (if (not (input-port? input-port))
                           "  current-runtime-input-port not configured"
                           (format "  input unused: ~v"
                                   (string-append*
                                    (let read-loop ([strs '()])
                                      (match (read-string 8 input-port)
                                        [(? eof-object?) (reverse strs)]
                                        [str (read-loop (cons str strs))])))))
                       (if (not (output-port? output-port))
                           "  current-runtime-output-port not configured"
                           (format "  output captured: ~v"
                                   (and (string-port? output-port)
                                        (get-output-string output-port)))))
          ;; No I/O.
          (format "error encountered during evaluation:\n\n~a\nmachine stack trace:\n~a"
                  (string-join (for/list ([str (in-list (string-split (exn-message e)
                                                                      "\n"))])
                                 (if (non-empty-string? str)
                                     (string-append "  " str)
                                     str))
                               "\n")
                  (format-stacktrace (current-context-emulator))))))))

;; A parameter that holds a procedure to be used when a non-exception value is
;; passed to [handle-error]. The procedure must accept two arguments: a value
;; and an output port. Note that the output port can be given [#f] (when I/O is
;; not configured in the emulator).
(define raise-handler
  (make-parameter
   (λ (v output-port)
     ;; Abort the command loop, returning the result value.
     (abort-command-loop
      ;; If I/O is configured, return the captured output as well.
      (if output-port
          (cons v
                (if (string-port? output-port)
                    (get-output-string output-port)
                    output-port))
          v)))))

;; Errors can be redirected to [handle-error] for graceful handling.
;;
;; If [e] is not an [exn?], it is treated like an [rax] value destined for a
;; graceful [exit].
;;
;; Otherwise, the pre-hook produces a formatted string with useful error
;; information. Then the main hook will raise an error with the formatted string
;; from the [pre-result].
(define-loop-command (handle-error e)
  ;; The main hook differentiates between exceptions and non-exceptions. The
  ;; former use the [exn?-handnler]. In other cases, [raise-handler] is used.
  #:main [(if (exn? e)
              ((exn?-handler)  e (current-runtime-output-port) (current-runtime-input-port))
              ((raise-handler) e (current-runtime-output-port)))])

;; Switches the emulator context. This requires the target emulator context
;; already exists.
(define-loop-command (switch emulator-name)
  #:main [(switch-context! emulator-name)]
  ;; After switching, start anew with the default command.
  #:post ['default])

;; TODO: Provide an option for specifying the emulator context name.
(define (run-emulator instructions runtime input-port output-port)
  ;; TODO: It'd be neat to try to get source location for the automatically
  ;; generated name instead of a random symbol.
  (initialize-context-and-switch! (gensym 'run-emulator)
                                  instructions
                                  runtime
                                  input-port
                                  output-port)
  (command-loop))

;; Run the emulator without I/O support.
(define (asm-emulate instructions [runtime empty-runtime])
  (run-emulator instructions runtime #f #f))

;; Run the emulator with I/O support.
(define (asm-emulate/io instructions
                        [runtime     empty-runtime]
                        [input-port  (open-input-string "")]
                        [output-port (open-output-string)])
  (run-emulator instructions runtime input-port output-port))
