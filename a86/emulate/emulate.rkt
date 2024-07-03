#lang racket

(provide current-emulator

         persist-current-emulator?

         current-emulator-before-thunk
         current-emulator-body-thunk
         current-emulator-after-thunk
         current-emulator-exit-handler
         current-emulator-exn?-handler
         current-emulator-raise-handler

         default-emulator-before-thunk
         default-emulator-before-thunk/io
         default-emulator-body-thunk
         default-emulator-body-thunk/io
         default-emulator-after-thunk
         default-emulator-after-thunk/io
         default-emulator-exit-handler
         default-emulator-exit-handler/io
         default-emulator-exn?-formatter
         default-emulator-exn?-formatter/io
         default-emulator-exn?-handler
         default-emulator-exn?-handler/io
         default-emulator-raise-handler
         default-emulator-raise-handler/io

         run-emulator
         reload-emulator
         exit-emulator

         asm-emulate
         asm-emulate/io)

(require "emulator.rkt"
         "exn.rkt"
         "runtime.rkt"
         "stacktrace.rkt"
         "../utility.rkt")

;; Whether to persist the [current-emulator] beyond the scope of [run-emulator].
(define persist-current-emulator? (make-parameter #f))

;; The continuation prompt currently in use by the emulator.
;;
;; NOTE: Do not export!
(define current-emulator-continuation-prompt-tag
  (make-parameter (default-continuation-prompt-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emulation Parameters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Called immediately after emulator initialization, but before the
;; [current-emulator-body-thunk] is called. Result discarded.
(define current-emulator-before-thunk  (make-parameter #f))
;; Called after [current-emulator-before-thunk]. This should handle the
;; operation of the emulator.
(define current-emulator-body-thunk    (make-parameter #f))
;; Called after [current-emulator-body-thunk], but while [current-emulator] is
;; still parameterized.
(define current-emulator-after-thunk   (make-parameter #f))
;; Called if [exit] is called during evaluation of the
;; [current-emulator-body-thunk].
(define current-emulator-exit-handler  (make-parameter #f))
;; Called if an [exn?] is raised during evaluation of the
;; [current-emulator-body-thunk].
(define current-emulator-exn?-handler  (make-parameter #f))
;; Called if a non-[exn?] is raised during evaluation of the
;; [current-emulator-body-thunk].
;;
;; NOTE: This can be useful for terminating evaluation early through the
;; runtime, e.g., to "raise exceptions" within the program being interpreted.
(define current-emulator-raise-handler (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default Parameter Values
;;
;; The names are [default-<foo>] for non-I/O emulation and [default-<foo>/io]
;; for I/O emulation, where [current-<foo>] is the name of the corresponding
;; parameter (defined above).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By default, nothing occurs before the body, but it's nice to have options!
(define default-emulator-before-thunk    void)
(define default-emulator-before-thunk/io void)

;; By default, the emulator is run until either the program halts or it takes
;; [emulator-step-count] steps.
(define default-emulator-body-thunk    (λ () (emulator-multi-step!)))
(define default-emulator-body-thunk/io (λ () (emulator-multi-step!)))

;; Both of the default after-thunks retrieve the value of [rax] and decode it to
;; a signed integer form, since that's what happens in our existing C-based
;; runtime system.
;;
;; The I/O version also returns the [current-runtime-output-port], and the two
;; values are [cons]ed. If the [current-runtime-output-port] is a
;; [string-port?], then the string representation of its contents is returned
;; instead.
(define default-emulator-after-thunk
  (λ ()
    (let ([r (emulator-register-ref 'rax)])
      (if (a86-value? r)
          (a86-value->signed-integer r)
          r))))
(define default-emulator-after-thunk/io
  (λ ()
    (let ([r (emulator-register-ref 'rax)]
          [p (current-runtime-output-port)])
      (cons (if (a86-value? r)
                (a86-value->signed-integer r)
                r)
            (if (string-port? p)
                (get-output-string p)
                p)))))

;; If the program is exited prematurely, an exception is raised.
(define default-emulator-exit-handler    (λ (v) (raise-user-error "program exited with status ~a" v)))
(define default-emulator-exit-handler/io (λ (v) (raise-user-error "program exited with status ~a" v)))

(define default-emulator-exn?-formatter
  (λ (e)
    (format "error encountered during evaluation:\n\n~a\nmachine stack trace:\n~a"
            (string-join (for/list ([str (in-list (string-split
                                                   (exn-message e)
                                                   "\n"))])
                           (if (non-empty-string? str)
                               (string-append "  " str)
                               str))
                         "\n")
            (format-stacktrace (current-emulator)))))
(define default-emulator-exn?-formatter/io
  (λ (e)
    (format "error encountered during I/O evaluation:\n\n  ~a\n\n  machine stack trace:\n~a\n~a\n~a"
             (string-join (for/list ([str (in-list (string-split
                                                    (exn-message e)
                                                    "\n"))])
                            (if (non-empty-string? str)
                                (string-append "  " str)
                                str))
                          "\n")
             (string-join (for/list ([str (in-list (string-split
                                                    (format-stacktrace (current-emulator))
                                                    "\n"))])
                            (if (non-empty-string? str)
                                (string-append "  " str)
                                str))
                          "\n")
             (if (not (input-port? (current-runtime-input-port)))
                 "  current-runtime-input-port not configured"
                 (format "  input unused: ~v"
                         (string-append*
                          (let read-loop ([strs '()])
                            (match (read-string 8 (current-runtime-input-port))
                              [(? eof-object?) (reverse strs)]
                              [str (read-loop (cons str strs))])))))
             (if (not (output-port? (current-runtime-output-port)))
                 "  current-runtime-output-port not configured"
                 (format "  output captured: ~v"
                         (and (string-port? (current-runtime-output-port))
                              (get-output-string (current-runtime-output-port))))))))

;; When an exception occurs during execution, the exception is reproduced and a
;; stack trace is given.
;;
;; The I/O version also shows what input was not consumed yet, and what output
;; was captured prior the exception being raised.
(define default-emulator-exn?-handler
  (λ (e) (raise-user-error (default-emulator-exn?-formatter e))))
(define default-emulator-exn?-handler/io
  (λ (e) (raise-user-error (default-emulator-exn?-formatter/io e))))

;; Non-exceptions that are raised are returned as the result of the emulation.
(define default-emulator-raise-handler
  (λ (v)
    ((exit-emulator) v)))
(define default-emulator-raise-handler/io
  (λ (v)
    (let ([p (current-runtime-output-port)])
      ((exit-emulator) (cons v
                             (if (string-port? p)
                                 (get-output-string p)
                                 p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Running the Emulator
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The basic interface for running the interpreter. The arguments are a list of
;; a86 instructions, an input port, and an output port. The operation of the
;; interpreter can be customized by parameterization.
;;
;;   1. The [current-emulator] is initialized with the [instructions].
;;   2. The runtime input and output ports are parameterized.
;;   3. The [current-emulator-before-thunk] is evaluated.
;;   4. The exit handler and exception handlers are parameterized.
;;   5. The [current-emulator-body-thunk] is evaluated within this scope.
;;   6. Outside of that scope but still within the scope where
;;      [current-emulator] is parameterized, the [current-emulator-after-thunk]
;;      is evaluated.
;;
;; If [persist-current-emulator?] contains a non-[#f] value, the
;; [current-emulator] parameter will remain set to the emulator used by this
;; interpretation after [run-emulator] has finished evaluating. This can be
;; useful for debugging.
;;
;; In normal operation, the result of [run-emulator] is the result of
;; [current-emulator-after-thunk]. If the program is [exit]ed, an [exn?] is
;; raised, or a non-[exn?] is raised, the result of [run-emulator] is the result
;; of [current-emulator-exit-handler], [current-emulator-exn?-handler], or
;; [current-emulator-raise-handler], respectively.
(define (run-emulator instructions input-port output-port)

  (parameterize ([current-emulator-continuation-prompt-tag
                  (make-continuation-prompt-tag 'run-emulator)])

    (define (emulator-proc instructions input-port output-port)
      (let ([emulator (and instructions
                           (initialize-emulator instructions))])
        (when (persist-current-emulator?)
          (current-emulator emulator))
        (let/cc exit-proc
          (parameterize ([current-emulator               emulator]
                         [current-runtime-input-port   input-port]
                         [current-runtime-output-port output-port]
                         [exit-emulator                 exit-proc])
            ;; TODO: Enable [exit-emulator] within [current-emulator-body-thunk]
            ;; and make the thunk loop by default. I think the only case where
            ;; we want the emulator to stop is manually anyway?
            ((current-emulator-before-thunk))
            (parameterize ([exit-handler
                            (λ args (apply (current-emulator-exit-handler) args))])
              (let loop ()
                (with-handlers ([exn:fail:a86:emulator:resume?
                                 (λ _ (loop))])
                  (with-handlers ([exn?
                                   (λ args (apply (current-emulator-exn?-handler) args))]
                                  [(λ _ #t)
                                   (λ args (apply (current-emulator-raise-handler) args))])
                    ((current-emulator-body-thunk))))))
            ((current-emulator-after-thunk))))))

    (define (emulator-prompt-handler instructions input-port output-port)
      (call-with-continuation-prompt emulator-proc
                                     (current-emulator-continuation-prompt-tag)
                                     emulator-prompt-handler
                                     instructions
                                     input-port
                                     output-port))

    (emulator-prompt-handler instructions input-port output-port)))

;; Like [run-emulator], but only works when an emulator is already running. This
;; is useful when you want to emulate multiple programs within the same
;; emulation environment.
(define (reload-emulator instructions input-port output-port)
  ((current-emulator-after-thunk))
  (if (eq? (current-emulator-continuation-prompt-tag)
           (default-continuation-prompt-tag))
      (raise-user-error 'reload-emulator "emulator not active; cannot reload")
      (abort-current-continuation (current-emulator-continuation-prompt-tag)
                                  instructions input-port output-port)))

;; Forces [run-emulator] to replace the current continuation (starting after the
;; beginning of the emulator evaluation). This is useful for preventing
;; [run-emulator] from calling the [current-emulator-after-thunk].
;;
;; NOTE: This parameter is only parameterized within the bodies of
;; [current-emulator-exit-handler], [current-emulator-exn?-handler], and
;; [current-emulator-raise-handler].
;;
;; TODO: Should this also be parameterized within the
;; [current-emulator-body-thunk]? And possibly the other two as well? For the
;; moment I think not, but I'm not sure it's the right call.
(define exit-emulator
  (make-parameter
   (λ _
     (raise-user-error 'exit-emulator "only available within an active emulator loop"))))

;; Calls [run-emulator] with the default parameters for non-I/O interpretation.
;; However, parameters can either be overridden by using a keyword argument or
;; by performing the parameterization in a surrounding scope.
(define (asm-emulate instructions
                     #:before   [  before-thunk #f]
                     #:during   [    body-thunk #f]
                     #:after    [   after-thunk #f]
                     #:on-exit  [  on-exit-proc #f]
                     #:on-exn   [   on-exn-proc #f]
                     #:on-raise [ on-raise-proc #f])
  (parameterize
      ([current-emulator-exit-handler  (or on-exit-proc  (current-emulator-exit-handler)  default-emulator-exit-handler)]
       [current-emulator-exn?-handler  (or on-exn-proc   (current-emulator-exn?-handler)  default-emulator-exn?-handler)]
       [current-emulator-raise-handler (or on-raise-proc (current-emulator-raise-handler) default-emulator-raise-handler)]
       [current-emulator-before-thunk  (or before-thunk  (current-emulator-before-thunk)  default-emulator-before-thunk)]
       [current-emulator-body-thunk    (or body-thunk    (current-emulator-body-thunk)    default-emulator-body-thunk)]
       [current-emulator-after-thunk   (or after-thunk   (current-emulator-after-thunk)   default-emulator-after-thunk)])
    (run-emulator instructions #f #f)))

;; Calls [run-emulator] with the default parameters for I/O interpretation.
;; However, parameters can either be overridden by using a keyword argument or
;; by performing the parameterization in a surrounding scope.
(define (asm-emulate/io instructions
                        input-port
                        [output-port (open-output-string)]
                        #:before   [  before-thunk #f]
                        #:during   [    body-thunk #f]
                        #:after    [   after-thunk #f]
                        #:on-exit  [  on-exit-proc #f]
                        #:on-exn   [   on-exn-proc #f]
                        #:on-raise [ on-raise-proc #f])
  (parameterize
      ([current-emulator-exit-handler  (or on-exit-proc  (current-emulator-exit-handler)  default-emulator-exit-handler/io)]
       [current-emulator-exn?-handler  (or on-exn-proc   (current-emulator-exn?-handler)  default-emulator-exn?-handler/io)]
       [current-emulator-raise-handler (or on-raise-proc (current-emulator-raise-handler) default-emulator-raise-handler/io)]
       [current-emulator-before-thunk  (or before-thunk  (current-emulator-before-thunk)  default-emulator-before-thunk/io)]
       [current-emulator-body-thunk    (or body-thunk    (current-emulator-body-thunk)    default-emulator-body-thunk/io)]
       [current-emulator-after-thunk   (or after-thunk   (current-emulator-after-thunk)   default-emulator-after-thunk/io)])
    (run-emulator instructions input-port output-port)))
