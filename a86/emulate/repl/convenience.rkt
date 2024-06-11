#lang racket

(require "../../instructions.rkt"
         "../state.rkt"
         "repl-state.rkt")

(define (diff-flags prev curr) #f)
(define (diff-registers prev curr) #f)
(define (diff-memory prev curr) #f)

(define (current-repl-instruction) #f)
(define (previous-repl-instruction) #f)

(define (deduce-diffs [state0 (previous-repl-emulator-state)] [statef (current-repl-emulator-state)])
  (unless (< (StepState-time-tick state0) (StepState-time-tick statef))
    (raise-user-error 'deduce-diffs
                      "states not diffable due to incompatible time ticks: ~v not < ~v"
                      (StepState-time-tick state0)
                      (StepState-time-tick statef)))
  #f)


;; TODO: I think the only way to fully capture all of the information I want is
;; to record register and flag transactions like we do with memory. I think this
;; should be mostly straightforward, though, since all the capabilities are
;; implemented for memory.


(define (deduce-flag-diffs pre-state post-state)
  #f)

(define (deduce-register-diffs pre-state post-state)
  #f)

(define (deduce-memory-diffs pre-state post-state)
  #f)

;; (define (instruction-register-uses instruction)
;;   (define (return #:rr [rr '()]
;;                   #:rw [rw '()]
;;                   #:fr [fr '()]
;;                   #:fw [fw '()])
;;     (list rr rw fr fw))
;;   (match instruction
;;     ;; Instruction       regs read          regs written       flags read          flags written
;;     [(Push r)    (return #:rr (list r 'rsp) #:rw 'rsp)]
;;     [(Pop  r)    (return #:rr 'rsp          #:rw (list r 'rsp))]
;;     [(Pushf)     (return #:rr 'rsp          #:rw 'rsp          #:fr '(OF CF SF ZF))]
;;     [(Popf)      (return #:rr 'rsp          #:rw 'rsp                              #:fw '(OF CF SF ZF))]
;;     [(Ret)       (return #:rr 'rsp          #:rw 'rsp)]
;;     [(Call _)    (return #:rr 'rsp          #:rw 'rsp)]
;;     [(Not r)     (return #:rr (list r)      #:rw (list r))]
;;     [(Add r1 r2) (return #:rr (list r1 r2)  #:rw (list r1) #:fw '(OF))]))

;; (define (instruction-register+flags-diffs instruction)
;;   (define (returns #:registers [registers '()]
;;                    #:flags [flags '()])
;;     (values registers flags))
;;   (match instruction
;;     [(Push _) (returns #:registers (list 'rsp))]
;;     [(Pop  r) (returns #:registers (list 'rsp r))]
;;     [(Ret)    (returns #:registers (list 'rsp))]
;;     [(Call l) (returns 'ip)]
;;     [(Not  r) (returns )]
;;     [_ #f]))

(define (current-repl-modified-flags) #f)
(define (current-repl-modified-registers) #f)
(define (current-repl-modified-addresses) #f)
