#lang racket

#|

User interaction with the emulator is handled through a few components. We
analyze the distinct modes of interaction to determine the architecture of the
overall system.

  1. Execution-only.

     :: (Listof a86) -> Answer

     The given instructions are executed and the value stored in [rax] is
     returned. Errors that occur are raised as Racket errors.

  2. REPL-only.

     :: <REPL loop>

     A REPL session is opened with no instructions loaded. The user must load a
     sequence of instructions to do anything of interest.

  3. Execute-then-REPL.

     :: (Listof a86) -> <REPL loop>

     The given instructions are executed. After execution terminates, the REPL
     is launched with the focus at the last state of the emulator.

  4. Execute in REPL.

     :: (Listof a86) -> <REPL loop>

     The given instructions are loaded into the emulator and the REPL is
     started. No steps are taken.

  5. Execute, REPL on error.

     :: (Listof a86) -> (or Answer <REPL loop>)

     The given instructions are executed. If execution terminates normally, the
     value stored in [rax] is returned. If an error occurs during execution, the
     REPL is launched with the focus on the last good state of the emulator
     (such that taking a step results in the error).

Now, we consider generalized shapes of interaction.

(1) is straightforward. The only generalization it needs is "what to do at the
end". The default operation is to return the value in [rax] (and the captured
output, if any).

(2) is also straightforward. There is no generalization; the REPL is launched
and the rest is on the user.

(3) and (5) generalize to the same shape: execute the code and (conditionally)
launch the REPL according to certain conditions.

Actually... (5) and (1) might be the same thing. Maybe execution has a post-step
analysis that can investigate the emulator machine state and do something as
needed. If errors were moved into the machine state, they could easily be
checked and raised or the state stepped back.

With the exception of (2), execution seems to have similar semantics to a [for]
loop. For example, maybe (1) could be implemented as:

  (for/last ([state (in-emulator (current-emulator))])
    )

  ;; If an initial emulator is desired, it must be [(current-emulator)].
  (do ([emulator (current-emulator)                       (current-emulator)]
       [state    (and emulator (emulator-state emulator)) (emulator-step emulator)])
      (((should-stop?) emulator state)
       ((final-result) emulator state))
    ((inspect) emulator state))

|#

;;;;;;;;;;;;

(provide pre-result
         main-result

         set-default-command!

         define-loop-command
         loop-command-out

         abort-command-loop
         command-loop)

(require racket/provide-syntax

         (for-syntax (submod "../utility.rkt" racket/syntax)

                     syntax/parse))

;; These parameters are filled during evaluation of a loop command.
(define pre-result  (make-parameter '()))
(define main-result (make-parameter '()))

;; The default command to run on start-up.
(define default-command (make-parameter #f))
(define (set-default-command! command-name)
  (default-command command-name))

;; This hash maps loop command names to their functions. This provides dynamic
;; lookup during loop evaluation.
(define loop-modes (make-hasheq))

;; Track the names of commands at compile-time to provide useful errors.
(define-for-syntax command-names (make-hasheq))

;; Track the default command name, if any.
(define-for-syntax default-command #f)

;; These format strings are used in a couple places. Defining them here ensures
;; they are kept consistent at all use-sites.
(define-values-for-syntax (
                           default-pre-hook-fmt-string
                           default-hook-fmt-string
                           default-post-hook-fmt-string
                           pre-hook-fmt-string
                           hook-fmt-string
                           post-hook-fmt-string)
  (values "default-pre-~a-hook"
          "default-~a-hook"
          "default-post-~a-hook"
          "pre-~a-hook"
          "~a-hook"
          "post-~a-hook"))

;;   (define-loop-command id
;;     [#:default]
;;     [#:pre  body ...+]
;;     [#:main body ...+]
;;     [#:post body ...+])
;;
;;   (define-loop-command (id ...+)
;;     [#:default]
;;     [#:pre  body ...+]
;;     [#:main body ...+]
;;     [#:post body ...+])
;;
;; A /loop command/ is the fundamental mechanism of the emulation driver. When
;; a loop command terminates execution, its result must indicate which loop
;; command to execute next (along with any necessary arguments). Loop commands
;; are executed within the context of a guiding loop construct such that none of
;; them executes in tail position.
;;
;; Each loop command has a main body as well as a pre-hook and post-hook body,
;; all of which accept the same arguments, if any.
;;
;; TODO: This implementation relies on symbolic equivalence of the command names
;; to the results returned from each step of the looping construct. A really
;; nice improvement would be to instead provide a form, e.g., [%name], that can
;; be "called" like a function and which expands to [(command-loop 'name)], but
;; which can be used to statically check that the referenced name is defined.
;;
;; TODO: If abstracted away from this emulator project, I think one should need
;; to define a loop and then define commands for that loop specifically, e.g.,
;;
;;   (define-command-loop my-great-loop ...)
;;   (define-loop-command my-great-loop command-name ...)
(define-syntax (define-loop-command stx)
  (syntax-parse stx
    [(_ (~and (~or* (~and name:id (~bind [(param 1) '()]))
                    (name:id param ...))
              ;; We don't allow defining a command named [default].
              (~fail #:when (eq? 'default (syntax-e #'name))
                     "cannot define command named 'default'")
              ;; Check if this command has been defined before.
              (~fail #:when (hash-has-key? command-names (syntax-e #'name))
                     "duplicate command name"))
        (~alt (~optional (~seq #:pre  ~! [pre-body  ...+]) #:defaults ([(pre-body  1) (list #'(void))]))
              (~optional (~seq #:main ~! [main-body ...+]) #:defaults ([(main-body 1) (list #'(void))]))
              (~optional (~seq #:post ~! [post-body ...+]) #:defaults ([(post-body 1) (list #'(main-result))]))
              (~optional (~seq #:default
                               ~!
                               (~fail #:when default-command
                                      (format "default command already exists: ~s" default-command))
                               (~do (set! default-command (syntax-e #'name)))
                               (~bind [default-command-stx #'(set-default-command! 'name)])))) ...)

     #:with default-pre-name  (format-id #'name default-pre-hook-fmt-string  #'name)
     #:with default-main-name (format-id #'name default-hook-fmt-string      #'name)
     #:with default-post-name (format-id #'name default-post-hook-fmt-string #'name)

     #:with pre-name  (format-id #'name pre-hook-fmt-string  #'name)
     #:with main-name (format-id #'name hook-fmt-string      #'name)
     #:with post-name (format-id #'name post-hook-fmt-string #'name)

     ;; Remember this command name so we don't use it again.
     (hash-set! command-names (syntax-e #'name) (length (syntax->list #'(param ...))))

     #'(begin
         ;; The default hook functions are usable externally.
         (define (default-pre-name  param ...) pre-body  ...)
         (define (default-main-name param ...) main-body ...)
         (define (default-post-name param ...) post-body ...)

         ;; Hooks are customizable as parameters.
         ;;
         ;; TODO: Maybe add a custom syntax for this? Could be nice to improve
         ;; error handling.
         (define pre-name  (make-parameter default-pre-name))
         (define main-name (make-parameter default-main-name))
         (define post-name (make-parameter default-post-name))

         ;; A mode command is implemented as a variadic function that applies
         ;; each of the pre-hook, main body, and post-hook to the given
         ;; arguments. The [pre-result] is bound in both the main and post-hook
         ;; bodies, and the [main-result] is bound in the post-hook body.
         (define (mode-command . args)
           (parameterize* ([pre-result  (apply (pre-name)  args)]
                           [main-result (apply (main-name) args)])
             (apply (post-name) args)))

         ;; Add the new mode command to the dynamic map of modes.
         (hash-set! loop-modes 'name mode-command)

         ;; If given [#:default], set this as the default command.
         (~? default-command-stx))]))

;; TODO: Is this necessary?
#;(define-syntax (redefine-loop-command stx)
  (syntax-parse stx
    [(_ (~and (~or* (~and name:id (~bind [(param 1) '()]))
                    (name:id param ...))
              (~fail #:when (eq? 'default (syntax-e #'name))
                     "cannot redefine command named 'default'")
              (~fail #:unless (hash-has-key? command-names (syntax-e #'name))
                     "not a defined command"))
        (~alt (~optional (~seq #:pre  ~! [pre-body  ...+]) #:defaults ([(pre-body  1) (list #'(void))]))
              (~optional (~seq #:main ~! [main-body ...+]) #:defaults ([(main-body 1) (list #'(void))]))
              (~optional (~seq #:post ~! [post-body ...+]) #:defaults ([(post-body 1) (list #'(main-result))]))) ...)

     ;; Update the command's argument count, if needed.
     (hash-set! command-names (syntax-e #'name) (length (syntax->list #'(param ...))))

     #'(begin
         )]))

;; Allows for easily exporting all the forms defined by [define-loop-command].
(define-provide-syntax (loop-command-out stx)
  (syntax-parse stx
    [(_ name:id ...)
     (let ([names (syntax->list #'(name ...))])
       (with-syntax
         ([(default-pre-name  ...) (format-ids names default-pre-hook-fmt-string)]
          [(default-main-name ...) (format-ids names default-hook-fmt-string)]
          [(default-post-name ...) (format-ids names default-post-hook-fmt-string)]
          [(pre-name          ...) (format-ids names pre-hook-fmt-string)]
          [(main-name         ...) (format-ids names hook-fmt-string)]
          [(post-name         ...) (format-ids names post-hook-fmt-string)])
         (syntax/loc stx
           (combine-out
            (combine-out default-pre-name
                         default-main-name
                         default-post-name
                         pre-name
                         main-name
                         post-name) ...))))]))

;; A parameter that should hold a procedure that accepts a [continuation?],
;; representing the continuation of the command loop.
;;
;; NOTE: Do not export!
(define current-abort-loop-proc (make-parameter #f))

;; Aborts the command loop by using [current-abort-loop-proc].
(define (abort-command-loop [result void])
  (let ([abort-proc (current-abort-loop-proc)])
    ;; Verify a command loop is running.
    (unless abort-proc
      ;; TODO: Custom error.
      (raise-user-error 'abort-command-loop "command loop not currently active"))
    ;; Pass the [result] to the abort procedure. If [result] is itself a
    ;; [procedure?], call it first.
    (abort-proc (if (procedure? result)
                    (result)
                    result))))

;; The command loop provides a structure for continuous execution with
;; customizable actions. New commands can be defined with [define-loop-command],
;; which expands the capabilities of the command loop. Additionally, loop
;; commands each feature various customizable components; see
;; [define-loop-command] for more information.
;;
;; Each execution of the loop command calls itself in tail position.
(define (command-loop [default-command (default-command)])

  (define (loop command . args)
    (if (eq? command 'default)
        (apply loop default-command args)
        (match (hash-ref loop-modes command #f)
          ;; Command does not exist.
          [#f
           ;; TODO: Custom error.
           (raise-user-error 'command-loop
                             "unexpected invocation: ~v"
                             `(command-loop ,command ,@args))]
          ;; Command exists; execute it and apply the command loop to the result.
          ;; If the result is a list, that list is passed by [apply] as though it
          ;; were a list of separate arguments.
          [mode-proc
           (let ([result (apply mode-proc args)])
             (if (list? result)
                 (apply loop result)
                 (loop result)))])))

  (unless default-command
    (raise-user-error 'command-loop
                      "no default command given;\n  ~a\n  ~a"
                      "either set the [default-command] parameter or pass a"
                      "command name (as a symbol) to [command-loop]"))

  (unless (symbol? default-command)
    (raise-argument-error 'command-loop
                          "symbol?"
                          default-command))

  ;; Begin the loop with the default command selected, but allow aborting the
  ;; loop as needed.
  ;;
  ;; NOTE: Users should only abort via [abort-command-loop].
  (let/cc abort-loop-proc
    (parameterize ([current-abort-loop-proc abort-loop-proc])
      (loop default-command))))
