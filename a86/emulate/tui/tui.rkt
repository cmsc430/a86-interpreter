#lang racket

(require "../../debug.rkt"
         "../runtime.rkt"

         "functionality.rkt"
         "term.rkt"
         "region-state.rkt"
         "state.rkt"

         "regions/main.rkt")

(module* main #f
  (define runtime (make-parameter #f name->runtime))
  (define input-string (make-parameter #f))
  (command-line
   #:once-each
   [("-r" "--runtime") rt
                       "Select the runtime to use"
                       (runtime (string->symbol rt))]
   [("-i" "--input-string") is
                            "A string to use for input when needed"
                            (input-string is)]
   [("--debug") "Enable debugging (not recommended)"
                (use-debug? #t)]
   #:args ()
   (when (use-debug?)
     (debug-on!))
   (parameterize ([current-runtime-output-port (open-output-nowhere)]
                  [current-runtime-input-port (and (input-string)
                                                   (open-input-string (input-string)))])
     (with-state-from-input (runtime)
       (with-term
         (parameterize ([current-region-state (fresh-region-state)])
           ;; Manually initialize the regions one-by-one in the order we want them.
           (header:initialize!)
           (instructions:initialize!)
           (registers:initialize!)
           (memory:initialize!)
           ;; Draw all the regions.
           (call-all 'redraw!)
           ;; Set up the initial info.
           (header:write-info "Ready for execution...")
           (when (use-debug?)
             (header:write-state!))
           ;; Set up an exit from the loop.
           (let/ec break
             ;; Begin the loop, but with the functionality set up.
             (parameterize ([tui-loop-break break]
                            [current-keymap default-keymap])
               (let loop ()
                 (with-handlers ([exn? handle-error])
                   (term:get-key!)
                   (handle-key (term:last-key)))
                 (loop))))))))))

#;(with-example-state
  (with-term
    (parameterize ([current-region-state (fresh-region-state)])
      ;; Manually initialize the regions one-by-one in the order we want them.
      #;(header:initialize!)
      (instructions:initialize!)
      (registers:initialize!)
      (memory:initialize!)
      ;; Draw all the regions.
      (call-all 'redraw!)
      ;; Set up the initial info.
      #;(header:write-info "Ready for execution...")
      #;(header:write-state!)
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
