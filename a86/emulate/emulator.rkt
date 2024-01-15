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
         emulator-step!
         emulator-step-count
         emulator-multi-step!
         emulator-step-back!
         emulator->flags
         emulator->registers
         emulator-flag-ref
         emulator-register-ref
         emulator-memory-ref
         emulator-memory-ref/32
         emulator-memory-ref/16
         emulator-memory-ref/8)

(require "../debug.rkt"
         "../exn.rkt"
         "../registers.rkt"
         "../utility.rkt"

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
         [_      (debug "program initialized")]
         [mem    (make-memory-from-program prog)]
         [_      (debug "memory initialized")]
         ;; We add a "return address" at the top of the stack, used for
         ;; signaling program termination.
         [_      (begin
                   (memory-set! mem (address-range-hi mem stack) 0 0)
                   (memory-set! mem
                                (lesser-word-aligned-address
                                 (address-range-hi mem stack))
                                0
                                end-of-program-signal))]
         [_      (debug "initial memory adjusted for end-of-program signal")]
         [ip     (address-range-hi mem text)]
         [_      (debug "initial instruction pointer set: ~v" ip)]
         [addrs  (compute-label-addresses prog ip)]
         [_      (debug "label addresses computed")]
         [state  (StepState 0
                            ip
                            fresh-flags
                            (register-set* fresh-registers
                                           'rsp (lesser-word-aligned-address
                                                 (address-range-hi mem stack))
                                           'rdi (address-range-lo mem heap))
                            (list))]
         [_      (debug "first state initialized")]
         [states (make-vector states-size-increment #f)])
    (vector-set! states 0 state)
    (Emulator states 0 mem addrs)))

(define (normalize-step-to-state-index who emulator step)
  (match emulator
    [(Emulator states index _ _)
     (match step
       [(? exact-nonnegative-integer?) step]
       [(? exact-integer?)
        (let ([new-index (+ index step)])
          (cond
            [(negative? new-index)
             (raise-a86-error who "negative state index: ~v" new-index)]
            [(>= new-index (vector-length states))
             (raise-a86-error who "too-great state index: ~v" new-index)]
            [else
             new-index]))]
       [#f index]
       [_ (raise-a86-error who "invalid step: ~v" step)])]))

(define (emulator-state [emulator #f] [step #f])
  (set! emulator (or emulator (current-emulator)))
  (unless emulator
    (error 'emulator-state "no emulator set!"))
  (vector-ref (Emulator-states emulator)
              (normalize-step-to-state-index 'emulator-state emulator step)))

(define (emulator-step! [emulator #f])
  (set! emulator (or emulator (current-emulator)))
  (match emulator
    [(Emulator states old-index memory labels->addresses)
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

;; The maximum number of steps can be parameterized.
(define emulator-step-count (make-parameter 1000))

(define (emulator-multi-step! [emulator #f])
  (set! emulator (or emulator (current-emulator)))
  (debug "labels->addresses:")
  (for ([(label address) (in-hash (Emulator-labels->addresses emulator))])
    (debug "  ~v\t~a" label (format-word address 'hex)))
  (debug "text:")
  (debug-memory-section (Emulator-memory emulator) text)
  (debug "")
  (parameterize ([trace-registers (list 'rax 'rsp)])
    (let recurse ([steps (emulator-step-count)]
                  [last-state (emulator-state emulator)])
      (when (not (zero? steps))
        (debug-memory-section (Emulator-memory emulator) stack)
        (debug-flags (emulator->flags emulator))
        (debug-registers (emulator->registers emulator))
        (emulator-step! emulator)
        (let ([new-state (emulator-state emulator)])
          (unless (eq? new-state last-state)
            (recurse (sub1 steps)
                     new-state)))))
    (debug "\n\n")))

;; NOTE: Cannot step backwards past the first state.
;; TODO: Should this raise an exception instead?
(define (emulator-step-back! [emulator #f])
  (set! emulator (or emulator (current-emulator)))
  (set-Emulator-current-index!
   emulator
   (max (sub1 (Emulator-current-index emulator))
        0)))

(define (emulator->flags [emulator #f] [step #f])
  (StepState-flags (emulator-state (or emulator (current-emulator)) step)))

(define (emulator->registers [emulator #f] [step #f])
  (StepState-registers (emulator-state (or emulator (current-emulator)) step)))

(define emulator-flag-ref
  (case-lambda
    [(flag)
     (emulator-flag-ref (current-emulator) flag)]
    [(emulator flag)
     (hash-ref (emulator->flags emulator) flag)]
    [(emulator step flag)
     (hash-ref (emulator->flags emulator step) flag)]))

(define emulator-register-ref
  (case-lambda
    [(register)
     (emulator-register-ref (current-emulator) register)]
    [(emulator register)
     (register-ref (emulator->registers emulator) register)]
    [(emulator step register)
     (register-ref (emulator->registers emulator step) register)]))

(define-syntax (define-emulator-memory-ref stx)
  (syntax-parse stx
    [(_ name:id byte-count:integer)
     #'(define name
         (case-lambda
           [(address)
            (name (current-emulator) #f address)]
           [(emulator address)
            (name emulator #f address)]
           [(emulator step address)
            (let ([tick (StepState-time-tick (emulator-state emulator step))])
              (memory-ref (Emulator-memory emulator) address tick byte-count))]))]))

(define-emulator-memory-ref emulator-memory-ref    8)
(define-emulator-memory-ref emulator-memory-ref/32 4)
(define-emulator-memory-ref emulator-memory-ref/16 2)
(define-emulator-memory-ref emulator-memory-ref/8  1)
