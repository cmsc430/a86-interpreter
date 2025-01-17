#lang racket

(provide emulator?

         add-context!
         initialize-context!
         switch-context!
         add-context-and-switch!
         initialize-context-and-switch!

         max-step-count

         current-context-name
         current-context-emulator
         current-context-runtime
         current-context-input-port
         current-context-output-port

         current-emulator-step!
         current-emulator-step-back!
         current-emulator-address-readable?
         current-emulator-address-writable?

         current-emulator-state
         current-emulator-state-index
         current-emulator-instruction-pointer
         current-emulator-instruction
         current-emulator-flags
         current-emulator-flag-ref
         current-emulator-registers
         current-emulator-register-ref
         current-emulator-memory
         current-emulator-memory-ref
         current-emulator-memory-ref/32
         current-emulator-memory-ref/16
         current-emulator-memory-ref/8
         current-emulator-memory-ref*
         current-emulator-memory-ref*/32
         current-emulator-memory-ref*/16
         current-emulator-memory-ref*/8

         previous-emulator-state
         previous-emulator-state-index
         previous-emulator-instruction-pointer
         previous-emulator-instruction
         previous-emulator-flags
         previous-emulator-flag-ref
         previous-emulator-registers
         previous-emulator-register-ref
         previous-emulator-memory-ref
         previous-emulator-memory-ref/32
         previous-emulator-memory-ref/16
         previous-emulator-memory-ref/8
         previous-emulator-memory-ref*
         previous-emulator-memory-ref*/32
         previous-emulator-memory-ref*/16
         previous-emulator-memory-ref*/8)

(require "../exn.rkt"
         "../registers.rkt"

         "memory.rkt"
         "program.rkt"
         "runtime.rkt"
         "step.rkt"
         "state.rkt"

         (for-syntax syntax/parse))

(module+ private
  (provide (struct-out Emulator)
           emulator-state))

(struct Emulator ([terminated?   #:mutable] ;; boolean?
                  [states        #:mutable] ;; [Vectorof StepState?]
                  [current-index #:mutable] ;; integer?
                  memory                    ;; Memory?
                  labels->addresses)        ;; [Hashof label? address?]
  #:transparent)
(define emulator? Emulator?)

;; The number of slots to add to an emulator's states vector when the creation
;; of a new state requires additional room.
(define states-size-increment 32)

;; Given a set of instructions, creates an [emulator?].
(define (initialize-emulator instructions)
  (let* ([prog   (make-program instructions)]
         [mem    (make-memory-from-program prog)]
         ;; We add a "return address" at the top of the stack, used for
         ;; signaling program termination.
         [_      (begin
                   (memory-set! mem (address-range-hi mem stack) 0 0)
                   (memory-set! mem
                                (address-range-hi mem stack)
                                0
                                end-of-program-signal))]
         [ip     (address-range-hi mem text)]
         [addrs  (compute-label-addresses prog ip)]
         [state  (StepState 0
                            ip
                            fresh-flags
                            (register-set* fresh-registers
                                           'rsp (address-range-hi mem stack)
                                           'rdi (address-range-lo mem heap))
                            (list)
                            (list)
                            (list))]
         [states (make-vector states-size-increment #f)])
    (vector-set! states 0 state)
    (Emulator #f states 0 mem addrs)))

;; Users often use the emulator in a relative fashion (e.g., "take two steps
;; forward" or "take one step back"), but the indices of states in the state
;; vector are absolute. This function performs the necessary conversion from a
;; number of steps to a state index, calculated relative to the current index.
(define (normalize-step-to-state-index who emulator step)
  (let ([states (Emulator-states emulator)]
        [index  (Emulator-current-index emulator)])
    (match step
      ;; If [step] is not negative, it should correspond directly to a state
      ;; index. Check that it isn't too big.
      [(? exact-nonnegative-integer?)
       (if (>= step (vector-length states))
           (raise-a86-error who "step greater than number of states: ~v" step)
           step)]
      ;; If [step] is negative, it's meant to be relative to the current index.
      ;; Check that the resulting index isn't negative.
      [(? exact-integer?)  ;; NOTE: Always negative.
       (let ([new-index (+ index step)])
         (if (negative? new-index)
             (raise-a86-error who "relative step results in invalid state: ~v" new-index)
             new-index))]
      ;; We allow [step] to be [#f] as a shorthand for retrieving the current
      ;; state index.
      [#f index]
      ;; Anything else is problematic.
      [_ (raise-a86-error who "invalid step: ~v" step)])))

;; Retrieves the currently selected state of the given emulator.
(define (emulator-state emulator [step #f])
  (unless emulator
    (error 'emulator-state "no emulator set!"))
  (vector-ref (Emulator-states emulator)
              (normalize-step-to-state-index 'emulator-state emulator step)))

;; The maximum number of steps any given emulator may take.
(define max-step-count (make-parameter 10000))

;; Moves the given [emulator] to the next state, if one is available. If the
;; emulator has terminated already or if the attempt to step fails (e.g.,
;; because the program has terminated), the emulator's state is not changed.
(define (emulator-step! emulator)
  (match emulator
    [(Emulator terminated? states old-index memory labels->addresses)
     ;; Sometimes we won't do anything at all.
     (cond
       ;; Don't do anything if this emulator was previously terminated.
       ;;
       ;; TODO: Custom error?
       [terminated?
        (void)]
       ;; Don't do anything if this emulator has taken the maximum number of
       ;; steps allowed.
       ;;
       ;; TODO: Custom error?
       [(>= old-index (max-step-count))
        (set-Emulator-terminated?! emulator #t)]
       ;; Otherwise, attempt to take a step.
       [else
        (let* ([new-index (add1 old-index)]
              [states-length (vector-length states)]
              ;; We check to see if we've already computed the next state.
              [existing-next-state (and (< new-index states-length)
                                        (vector-ref states new-index))])
         ;; Have we already computed the next state?
         (if existing-next-state
             ;; Yes, so we just update the index.
             (set-Emulator-current-index! emulator new-index)
             ;; No, so we have to try to take a step. The old state is retrieved
             ;; and used as the basis for the next one.
             (let* ([old-state (vector-ref states old-index)]
                    [new-state (step/manual old-state memory labels->addresses)])
               ;; Check whether we actually took a step.
               (if new-state
                   ;; If we now have a new state, make sure we have room for it.
                   (begin
                     ;; If necessary, we extend the vector of states. The vector
                     ;; is grown by [states-size-increment] each time more space
                     ;; is needed.
                     (when (>= new-index states-length)
                       (let ([new-states (make-vector
                                          (+ states-length states-size-increment)
                                          #f)])
                         (vector-copy! new-states 0 states)
                         (set-Emulator-states! emulator new-states)
                         (set! states new-states)))
                     ;; Save the new state.
                     (vector-set! states new-index new-state)
                     ;; Switch to that new state.
                     (set-Emulator-current-index! emulator new-index))
                   ;; Otherwise, mark this emulator as terminated and take no
                   ;; other actions.
                   (set-Emulator-terminated?! emulator #t)))))])]))

;; NOTE: Cannot step backwards past the first state.
;;
;; TODO: Should this raise an exception if we do attempt to step back too far?
(define (emulator-step-back! emulator)
  (set-Emulator-current-index! emulator
                               (max (sub1 (Emulator-current-index emulator))
                                    0)))

(define-syntax (define-emulator-memory-ref stx)
  (syntax-parse stx
    [(_ name:id byte-count:integer)
     #'(define (name emulator address [step #f])
         (let ([tick (StepState-time-tick (emulator-state emulator step))])
           (memory-ref (Emulator-memory emulator) address tick byte-count)))]))

(define-emulator-memory-ref emulator-memory-ref    8)
(define-emulator-memory-ref emulator-memory-ref/32 4)
(define-emulator-memory-ref emulator-memory-ref/16 2)
(define-emulator-memory-ref emulator-memory-ref/8  1)

(define-syntax (define-emulator-memory-ref* stx)
  (syntax-parse stx
    [(_ name:id byte-count:integer)
     #'(define (name emulator address n [step #f])
         (let ([tick (StepState-time-tick (emulator-state emulator step))])
           (memory-ref* (Emulator-memory emulator) address n tick byte-count)))]))

(define-emulator-memory-ref* emulator-memory-ref*    8)
(define-emulator-memory-ref* emulator-memory-ref*/32 4)
(define-emulator-memory-ref* emulator-memory-ref*/16 2)
(define-emulator-memory-ref* emulator-memory-ref*/8  1)

;; TODO: Add functionality to test readability based on [step] (consider
;; dynamic memory allocation).
(define (emulator-address-readable? emulator address [step #f])
  (address-readable? (Emulator-memory emulator) address))

;; TODO: Add functionality to test writability based on [step] (consider
;; dynamic memory allocation).
(define (emulator-address-writable? emulator address [step #f])
  (address-writable? (Emulator-memory emulator) address))

(define (emulator-state->flags state)
  (StepState-flags state))

(define (emulator-state->registers state)
  (StepState-registers state))

(define (emulator-state-flag-ref state flag)
  (hash-ref (emulator-state->flags state) flag))

(define (emulator-state-register-ref state register)
  (register-ref (emulator-state->registers state) register))

(define (emulator-state->instruction-pointer state)
  (StepState-ip state))

;; An [EmulatorContext] contains everything necessary to run the emulator:
;;
;;   name         The name of the context; used for switching to or inspecting.
;;   emulator     The actual [emulator?] object for this context.
;;   runtime      A [runtime?] to be used during execution.
;;   input-port   An [input-port?] to use when the [runtime] attempts to obtain
;;                user input during execution, or [#f] to disable.
;;   output-port  Like [input-port] but with an [output-port?] for output.
(struct EmulatorContext (name emulator runtime input-port output-port))
(define context? EmulatorContext?)

;; All of the initialized [EmulatorContext]s and the current context, if any.
(define contexts        (make-parameter '()))
(define current-context (make-parameter #f))

;; Adds a new context to the [contexts]. This action is thread-permanent.
(define (add-context! name
                      emulator
                      [runtime     empty-runtime]
                      [input-port  #f]
                      [output-port #f])
  (let ([runtime (if (symbol? runtime)
                     (or (name->runtime runtime)
                         (raise-a86-error 'add-context! "unable to find runtime named ~s" runtime))
                     runtime)])
    (let ([new-context (EmulatorContext name emulator runtime input-port output-port)])
      (contexts (cons new-context (contexts))))))

;; Adds a new context to the [contexts] with a new emulator. This action is
;; thread-permanent.
(define (initialize-context! name
                             instructions
                             [runtime     empty-runtime]
                             [input-port  #f]
                             [output-port #f])
  (let ([emulator (initialize-emulator instructions)])
    (add-context! name
                  emulator
                  runtime
                  input-port
                  output-port)))

;; Changes the [current-context] based on the [name-or-context].
;;
;; If [name-or-context] is a [context?], it is used directly. If
;; [name-or-context] is a [symbol?], it is assumed to be the name of a context
;; in the [contexts].
;;
;; If either [name-or-context] is not a [context?] or [symbol?] or if it is a
;; [symbol?] but the name does not correspond to a context defined in the
;; [contexts], an error is raised.
(define (switch-context! name-or-context)
  (match (match name-or-context
           [(? context? context) context]
           [(? symbol? context-name)
            (findf (λ (context) (eq? context-name (EmulatorContext-name context)))
                   (contexts))]
           [_ (raise-user-error 'switch-context! "not a context or context name: ~v" name-or-context)])
    [#f (raise-user-error 'switch-context! "no such context: ~a" name-or-context)]
    [context
     ;; Update the context appropriately.
     (current-context context)
     ;; And also update the runtime ports.
     ;;
     ;; TODO: The [runtime] parameters were written before this driver framework
     ;; was established, so there's a bit of extra cruft around them. I'm
     ;; delaying cleaning it up just to get things working, but I want to come
     ;; back to this and figure out if there's a better way to arrange these so
     ;; it feels less redundant.
     (current-runtime             (current-context-runtime))
     (current-runtime-input-port  (current-context-input-port))
     (current-runtime-output-port (current-context-output-port))]))

;; Defines a new [EmulatorContext] and switches to it. The arguments are the
;; same as for [add-context!].
(define (add-context-and-switch! . args)
  (apply add-context! args)
  (switch-context! (car (contexts))))

;; Defines a new [EmulatorContext] with a new emulator and switches to it. The
;; arguments are the same as for [initialize-context!].
(define (initialize-context-and-switch! . args)
  (apply initialize-context! args)
  (switch-context! (car (contexts))))

;; Parameters that provide direct access to the components of the current
;; [EmulatorContext].
(define (current-context-name)        (EmulatorContext-name        (current-context)))
(define (current-context-emulator)    (EmulatorContext-emulator    (current-context)))
(define (current-context-runtime)     (EmulatorContext-runtime     (current-context)))
(define (current-context-input-port)  (EmulatorContext-input-port  (current-context)))
(define (current-context-output-port) (EmulatorContext-output-port (current-context)))

;; Convenience functions for accessing the current state in the emulator.

(define (current-emulator-step!)
  (emulator-step! (current-context-emulator)))

(define (current-emulator-step-back!)
  (emulator-step-back! (current-context-emulator)))

(define (current-emulator-address-readable? address)
  (emulator-address-readable? (current-context-emulator) address))

(define (current-emulator-address-writable? address)
  (emulator-address-writable? (current-context-emulator) address))

(define (current-emulator-state)
  (emulator-state (current-context-emulator)))

(define (current-emulator-state-index)
  (normalize-step-to-state-index 'current-emulator-state-index (current-context-emulator) #f))

(define (current-emulator-instruction-pointer)
  (emulator-state->instruction-pointer (current-emulator-state)))

(define (current-emulator-instruction)
  (emulator-memory-ref (current-context-emulator) (emulator-state->instruction-pointer (current-emulator-state))))

(define (current-emulator-flags)
  (emulator-state->flags (current-emulator-state)))

(define (current-emulator-flag-ref flag)
  (emulator-state-flag-ref (current-emulator-state) flag))

(define (current-emulator-registers)
  (emulator-state->registers (current-emulator-state)))

(define (current-emulator-register-ref register)
  (emulator-state-register-ref (current-emulator-state) register))

(define (current-emulator-memory)
  (Emulator-memory (current-context-emulator)))

(define (current-emulator-memory-ref address)
  (emulator-memory-ref (current-context-emulator) address))

(define (current-emulator-memory-ref/32 address)
  (emulator-memory-ref/32 (current-context-emulator) address))

(define (current-emulator-memory-ref/16 address)
  (emulator-memory-ref/16 (current-context-emulator) address))

(define (current-emulator-memory-ref/8 address)
  (emulator-memory-ref/8 (current-context-emulator address)))

(define (current-emulator-memory-ref* address n)
  (emulator-memory-ref* (current-context-emulator) address n))

(define (current-emulator-memory-ref*/32 address n)
  (emulator-memory-ref*/32 (current-context-emulator) address n))

(define (current-emulator-memory-ref*/16 address n)
  (emulator-memory-ref*/16 (current-context-emulator) address n))

(define (current-emulator-memory-ref*/8 address n)
  (emulator-memory-ref*/8 (current-context-emulator) address n))

;; Convenience functions for accessing the previous state in the emulator.

(define (previous-emulator-state)
  (emulator-state (current-context-emulator) -1))

(define (previous-emulator-state-index)
  (normalize-step-to-state-index 'previous-emulator-state-index (current-context-emulator) -1))

(define (previous-emulator-instruction-pointer)
  (emulator-state->instruction-pointer (previous-emulator-state)))

(define (previous-emulator-instruction)
  (emulator-memory-ref (current-context-emulator) (emulator-state->instruction-pointer (previous-emulator-state)) -1))

(define (previous-emulator-flags)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator-state->flags (previous-emulator-state))))

(define (previous-emulator-flag-ref flag)
  (with-handlers ([exn:fail:a86? (λ _ 'no-result)])
    (emulator-state-flag-ref (previous-emulator-state) flag)))

(define (previous-emulator-registers)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator-state->registers (previous-emulator-state))))

(define (previous-emulator-register-ref register)
  (with-handlers ([exn:fail:a86? (λ _ #f)])
    (emulator-state-register-ref (previous-emulator-state) register)))

(define (previous-emulator-memory-ref address)
  (emulator-memory-ref (current-context-emulator) address -1))

(define (previous-emulator-memory-ref/32 address)
  (emulator-memory-ref/32 (current-context-emulator) address -1))

(define (previous-emulator-memory-ref/16 address)
  (emulator-memory-ref/16 (current-context-emulator) address -1))

(define (previous-emulator-memory-ref/8 address)
  (emulator-memory-ref/8 (current-context-emulator) address -1))

(define (previous-emulator-memory-ref* address n)
  (emulator-memory-ref* (current-context-emulator) address n -1))

(define (previous-emulator-memory-ref*/32 address n)
  (emulator-memory-ref*/32 (current-context-emulator) address n -1))

(define (previous-emulator-memory-ref*/16 address n)
  (emulator-memory-ref*/16 (current-context-emulator) address n -1))

(define (previous-emulator-memory-ref*/8 address n)
  (emulator-memory-ref*/8 (current-context-emulator) address n -1))
