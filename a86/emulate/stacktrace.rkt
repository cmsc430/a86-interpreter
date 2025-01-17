#lang racket

(provide display-stacktrace
         display-instructions
         format-stacktrace)

(require "../debug.rkt"
         "../registers.rkt"
         "../utility.rkt"

         "emulator.rkt"
         "memory.rkt"
         "state.rkt"

         (submod "emulator.rkt" private))

(define default-stacktrace-expanded-count 25)
(define default-stacktrace-condensed-count 50)

(define (display-stacktrace [emulator #f]
                            [expanded default-stacktrace-expanded-count]
                            [condensed default-stacktrace-condensed-count])
  (display (format-stacktrace (or emulator
                                  (current-context-emulator))
                              expanded condensed)))

(define (display-instructions [emulator #f])
  (display (format-instructions (or emulator
                                    (current-context-emulator)))))

(define (format-instructions emulator)
  (let* ([memory (Emulator-memory emulator)]
         [text-hi (address-range-hi memory text)]
         [text-lo (address-range-lo memory text)])
    (string-join
     (for/list ([address (in-inclusive-range text-hi text-lo -8)])
       (format "[~a]    ~v"
               (format-word address 'hex)
               (memory-ref memory address)))
     "\n")))

;; Formats a string representing a stack trace of the [emulator], starting from
;; the latest state. The last [expanded] states will be rendered in a more
;; verbose form, while the previous [condensed] states before those will be
;; rendered in a concise form. Other states, if they exist, will not be
;; rendered.
(define (format-stacktrace emulator [expanded 25] [condensed 50])
  (parameterize ([debug-on? #f])
    (let* ([states         (Emulator-states        emulator)]
           [current-index  (Emulator-current-index emulator)]
           [memory         (Emulator-memory        emulator)]
           [max-tick-width (string-length (format "~a" current-index))])
      (string-join
       (for/list ([idx+exp? (for/list ([idx (in-inclusive-range
                                             (- current-index expanded condensed)
                                             current-index)]
                                       [exp? (in-list (append (make-list condensed #f)
                                                              (make-list expanded  #t)))]
                                       #:when (positive? idx))
                              (cons idx exp?))])
         (let* ([succ-idx (car idx+exp?)]
                [base-idx (sub1 succ-idx)]
                [exp? (cdr idx+exp?)])
           (format-steptrace memory
                             (vector-ref states base-idx)
                             (vector-ref states succ-idx)
                             max-tick-width
                             exp?)))
       ""))))

;; Formats a single state from memory as a string. If not [expanded?], the
;; string will take one line. If [expanded?], the string will take multiple
;; lines.
;;
;; The output looks something like the following, with vertical bars along the
;; left edge to show indentation:
;;
;;   9: [_SZc] [xxxxxxxxx] (Mov 'rax 12)
;;
;; TODO: The [expanded?] functionality needs to be implemented. I actually think
;; things would be a lot easier if the emulator also tracked changes along the
;; way. It's easy to work out changes to the flags based on the before and after
;; values, but doing the same thing with memory or registers is somewhat
;; trickier. Tracking changes in parallel to execution would make it much more
;; straightforward to reproduce this information on-demand.
(define (format-steptrace memory
                          base-state    ;; Chronologically earlier state.
                          succ-state    ;; State dependent upon [base-state].
                          [tick-width 0]
                          [expanded? #f])
  (match* (base-state succ-state)
    [((StepState _         ip base-flags base-registers _ _ _)
      (StepState time-tick _  succ-flags succ-registers _ _ _))
     (let ([instruction (memory-ref memory ip)])
       (string-append
        ;; First line.
        (format "~a: [~a] [~a] ~v\n"
                (~r time-tick #:min-width tick-width)
                (format-flagchange base-flags succ-flags)
                (format-word ip 'hex)
                instruction)
        ;; Additional lines, if expanded.
        (if (not expanded?)
            ""
            "")))]))  ;; TODO

(define (format-flagchange base-flags succ-flags)
  (string-join
   (for/list ([flag flag-names])
     (if (eq? (hash-ref base-flags flag)
              (hash-ref succ-flags flag))
         "_"
         ((if (hash-ref succ-flags flag)
              (λ (x) x)
              string-downcase)
          (substring (symbol->string flag) 0 1))))
   ""))
