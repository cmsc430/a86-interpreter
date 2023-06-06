#lang racket

(require "exn.rkt"
         "functionality.rkt"
         "term.rkt"
         "regions.rkt"
         "region-state.rkt"
         "state.rkt")

(with-example-state
  (with-term
    (parameterize ([current-region-state (fresh-region-state)])
      ;; Manually initialize the regions one-by-one in the order we want them.
      (header:initialize!)
      (instructions:initialize!)
      ;; Draw all the regions.
      (call-all 'redraw!)
      ;; Set up the initial info.
      (header:write-info "Ready for execution...")
      (header:write-state!)
      ;; Set up an exit from the loop.
      (let/ec break
        ;; Begin the loop, but with the functionality set up.
        (parameterize ([tui-loop-break break]
                       [current-keymap default-keymap])
          (let loop ()
            (with-handlers ([exn? handle-error])
              (term:get-key!)
              (handle-key (term:last-key)))
            (loop)))))))
