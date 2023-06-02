#lang racket

(require "term.rkt"
         "region.rkt"
         "regions.rkt"
         "region-state.rkt"
         "state.rkt"

         "../../ast.rkt")

(with-example-state
  (with-term
    (parameterize ([current-region-state (fresh-region-state)])
      (header:initialize!)
      (displayln (format "header: ~v" (region-coords header:region)))
      (instructions:initialize!)
      (let/ec break
        (let loop ()
          (term:get-key!)
          (regions:redraw-all!)
          (match (term:last-key)
            [(or #\q #\Q)
             (term:clear-screen)
             (break)]
            [_ (loop)]))))))
