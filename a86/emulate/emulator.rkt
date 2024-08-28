#lang racket

;; NOTE: As I originally set out to implement this abstraction, I found myself
;; revising the internal representation of states many times. This is due to a
;; number of things, but primarily I wanted to plan ahead for some features that
;; I intend for the emulator to support, such as being able to arbitrarily
;; change register or memory values at runtime and then resuming execution in
;; either direction.
;;
;; The regular forward/backward stepping functionality is easy to accommodate:
;; forward steps can be run as needed, and backward steps can retrieve past
;; states and re-load them as the "current" state.
;;
;; But if we allow arbitrary changes of values... things become complicated. I
;; had originally planned to set up "Executions" --- devices that track a
;; related sequence of states, and then an Emulator would contain all its
;; Executions (between which the user may toggle as needed, or else fork a new
;; one by committing new value changes).
;;
;; However, I eventually realized that the current implementation of the machine
;; simply will not support this functionality. Because memory is handled as a
;; separate state-containing device, I would need to track individual memory
;; cells from the Execution. Not only is this overly complex in itself, but if
;; you factor in the existing complexity of the different modes of memory update
;; it becomes downright absurd.
;;
;; TODO: I think I have an idea for how to resolve this all in the future, but I
;; won't implement it right now.
;;
;; The idea is to take inspiration from spreadsheets. Instead of writing down
;; all the past memory values for a given address and saving all the past
;; values for registers in the [StepState]s, I think a system can be set up to
;; compute the values on-demand via functions. This will require an overhaul of
;; both the state system as well as the memory system, such that all "memory
;; locations" (including registers) are aware of other areas of the system that
;; they touch. Either that, or the dependents can be made lazy with the
;; expectation that they have to fetch up-to-date values when needed. (I think
;; this sounds better, now that I write it out, but I'll think on it.)
;;
;; Since this is beyond the scope of the current system, I think I'll stick to
;; just steppable debugging without supporting live value changes. It will be a
;; system for understanding rather than experimentation. But I would very much
;; like to rewrite it all to handle this additional functionality at some point.

(provide current-emulator
         initialize-emulator
         emulator?

         current-emulator-step!
         current-emulator-multi-step!
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
         previous-emulator-memory-ref*/8

         (rename-out [max-step-count emulator-step-count]))

(require "../exn.rkt"
         "../registers.rkt"

         "memory.rkt"
         "program.rkt"
         "step.rkt"
         "state.rkt"

         (for-syntax syntax/parse))

(module+ private
  (provide (struct-out Emulator)
           emulator-state))

(define current-emulator (make-parameter #f))

(struct Emulator ([states        #:mutable] ;; [Vectorof StepState?]
                  [current-index #:mutable] ;; integer?
                  memory                    ;; Memory?
                  labels->addresses)        ;; [Hashof label? address?]
  #:transparent)

(define emulator? Emulator?)

(define states-size-increment 32)

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
    (Emulator states 0 mem addrs)))

(define (normalize-step-to-state-index who emulator step)
  (match emulator
    [(Emulator states index _ _)
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
       [_ (raise-a86-error who "invalid step: ~v" step)])]))

(define (emulator-state emulator [step #f])
  (unless emulator
    (error 'emulator-state "no emulator set!"))
  (vector-ref (Emulator-states emulator)
              (normalize-step-to-state-index 'emulator-state emulator step)))

(define (emulator-step! emulator)
  (match emulator
    [(Emulator states old-index memory labels->addresses)
     ;; TODO: Add new [exn?] for attempting to take a step when no steps can be
     ;; taken, and maybe add a flag to [Emulator] that indicates whether steps
     ;; can still be taken. When no steps can be taken, raise the exception here
     ;; as well, and it should be handled elsewhere.
     (let* ([new-index (add1 old-index)]
            [states-length (vector-length states)]
            ;; We check to see if we've already computed the next state.
            [existing-next-state (and (< new-index states-length)
                                      (vector-ref states new-index))])
       ;; If the next state does not already exist, we need to build a new one.
       (unless existing-next-state
         ;; The old state is retrieved and used as the basis for the next step.
         (let* ([old-state (vector-ref states old-index)]
                [new-state (step/manual old-state memory labels->addresses)])
           ;; If necessary, we extend the vector of states. The vector is grown
           ;; by [states-size-increment] each time more space is needed.
           (when (>= new-index states-length)
             (let ([new-states (make-vector
                                (+ states-length states-size-increment)
                                #f)])
               (vector-copy! new-states 0 states)
               (set-Emulator-states! emulator new-states)
               (set! states new-states)))
           (vector-set! states new-index new-state)))
       (set-Emulator-current-index! emulator new-index))]))

;; Steps the emulator until either the new state is identical to the old state
;; or [exn:fail:a86:emulator:out-of-steps] is raised, then returns politely.
(define (emulator-multi-step! emulator)
  (let recurse ([last-state (emulator-state emulator)])
    (emulator-step! emulator)
    (let ([new-state (emulator-state emulator)])
      (unless (eq? new-state last-state)
        (recurse new-state)))))

;; NOTE: Cannot step backwards past the first state.
;; TODO: Should this raise an exception instead?
(define (emulator-step-back! emulator)
  (set-Emulator-current-index!
   emulator
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


;; Convenience functions for accessing the current state in the emulator.

(define (current-emulator-step!)
  (emulator-step! (current-emulator)))

(define (current-emulator-multi-step!)
  (emulator-multi-step! (current-emulator)))

(define (current-emulator-step-back!)
  (emulator-step-back! (current-emulator)))

(define (current-emulator-address-readable? address)
  (emulator-address-readable? (current-emulator) address))

(define (current-emulator-address-writable? address)
  (emulator-address-writable? (current-emulator) address))

(define (current-emulator-state)
  (emulator-state (current-emulator)))

(define (current-emulator-state-index)
  (normalize-step-to-state-index 'current-emulator-state-index (current-emulator) #f))

(define (current-emulator-instruction-pointer)
  (emulator-state->instruction-pointer (current-emulator-state)))

(define (current-emulator-instruction)
  (emulator-memory-ref (current-emulator) (emulator-state->instruction-pointer (current-emulator-state))))

(define (current-emulator-flags)
  (emulator-state->flags (current-emulator-state)))

(define (current-emulator-flag-ref flag)
  (emulator-state-flag-ref (current-emulator-state) flag))

(define (current-emulator-registers)
  (emulator-state->registers (current-emulator-state)))

(define (current-emulator-register-ref register)
  (emulator-state-register-ref (current-emulator-state) register))

(define (current-emulator-memory)
  (Emulator-memory (current-emulator)))

(define (current-emulator-memory-ref address)
  (emulator-memory-ref (current-emulator) address))

(define (current-emulator-memory-ref/32 address)
  (emulator-memory-ref/32 (current-emulator) address))

(define (current-emulator-memory-ref/16 address)
  (emulator-memory-ref/16 (current-emulator) address))

(define (current-emulator-memory-ref/8 address)
  (emulator-memory-ref/8 (current-emulator address)))

(define (current-emulator-memory-ref* address n)
  (emulator-memory-ref* (current-emulator) address n))

(define (current-emulator-memory-ref*/32 address n)
  (emulator-memory-ref*/32 (current-emulator) address n))

(define (current-emulator-memory-ref*/16 address n)
  (emulator-memory-ref*/16 (current-emulator) address n))

(define (current-emulator-memory-ref*/8 address n)
  (emulator-memory-ref*/8 (current-emulator) address n))


;; Convenience functions for accessing the previous state in the emulator.

(define (previous-emulator-state)
  (emulator-state (current-emulator) -1))

(define (previous-emulator-state-index)
  (normalize-step-to-state-index 'previous-emulator-state-index (current-emulator) -1))

(define (previous-emulator-instruction-pointer)
  (emulator-state->instruction-pointer (previous-emulator-state)))

(define (previous-emulator-instruction)
  (emulator-memory-ref (current-emulator) (emulator-state->instruction-pointer (previous-emulator-state)) -1))

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
  (emulator-memory-ref (current-emulator) address -1))

(define (previous-emulator-memory-ref/32 address)
  (emulator-memory-ref/32 (current-emulator) address -1))

(define (previous-emulator-memory-ref/16 address)
  (emulator-memory-ref/16 (current-emulator) address -1))

(define (previous-emulator-memory-ref/8 address)
  (emulator-memory-ref/8 (current-emulator) address -1))

(define (previous-emulator-memory-ref* address n)
  (emulator-memory-ref* (current-emulator) address n -1))

(define (previous-emulator-memory-ref*/32 address n)
  (emulator-memory-ref*/32 (current-emulator) address n -1))

(define (previous-emulator-memory-ref*/16 address n)
  (emulator-memory-ref*/16 (current-emulator) address n -1))

(define (previous-emulator-memory-ref*/8 address n)
  (emulator-memory-ref*/8 (current-emulator) address n -1))
