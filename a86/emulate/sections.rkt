#lang racket

(provide text rodata data bss heap stack
         read-only-sections
         read-write-sections
         static-sections
         dynamic-sections
         downward-sections
         upward-sections
         initialize-static-sections

         (all-from-out "section.rkt"))

(require "../debug.rkt"
         "../utility.rkt"

         "section.rkt")

;; These definitions for the section names simply protect against typos when
;; writing symbols, but we also establish some conventions around the use of
;; each section:
;;
;;                  text  rodata  data  bss  heap  stack
;;                +--------------------------------------
;;   writable?    |                x     x    x     x
;;   static?      |  x     x       x     x
;;   grows down?  |                                 x
;;
;; The below defined lists can be used to dynamically query a given section for
;; its intended use.
(define-values (                   text  rodata  data  bss  heap  stack )
  (values                         'text 'rodata 'data 'bss 'heap 'stack ))
(define read-only-sections  (list  text  rodata                         ))
(define read-write-sections (list                data  bss  heap  stack ))
(define static-sections     (list  text  rodata  data  bss              ))
(define dynamic-sections    (list                           heap  stack ))
(define downward-sections   (list                                 stack ))
(define upward-sections     (list  text  rodata  data  bss  heap        ))

;; Sets up the static sections, which are allocated only enough space to
;; accommodate their initial contents. Each section is set in memory directly
;; above the section before it, beginning at the [lo-address].
;;
;; Returns two values: the next low address that can be used outside of the
;; allocated sections, and an association list mapping address ranges to a pair
;; of the section's name and its corresponding [Section?].
(define/debug (initialize-static-sections lo-address content-pairs)
  (for/fold ([lo-address lo-address]
             [ranges->sections (list)])
            ([content-pair content-pairs])
    (match content-pair
      [(cons name contents)
       (if (not (empty? contents))
           (let* ([hi-address (+ lo-address
                                 (* word-size-bytes
                                    (sub1 (length contents))))]
                  [section (make-static-section contents)]
                  [address-range (list lo-address hi-address)])
             (values (greater-word-aligned-address hi-address)
                     (cons (cons address-range (cons name section))
                           ranges->sections)))
           (values lo-address
                   ranges->sections))])))
