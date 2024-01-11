;; This implementation of key-reading is inspired by
;; [charterm](https://docs.racket-lang.org/charterm/index.html), but has been
;; simplified to suit the lesser needs of this project.
#lang racket

(provide current-default-tty-path
         current-default-buffer-size
         current-default-timeout-seconds
         with-tty
         stty*
         stty*/capture
         with-raw-input
         with-cooked-input
         with-original-settings
         tty:size
         tty:read-byte
         tty:read-byte/timeout
         tty:read-bytes
         tty:read-bytes/timeout
         tty:write-byte
         tty:write-bytes
         tty:write-subbytes)

(require (for-syntax syntax/parse))

;; Where [stty] is located.
(define stty-path (find-executable-path "stty"))
;; OSes don't agree on the capitalization of this option, so we check.
(define stty-f-arg-string
  (case (system-type 'os*)
    [(macosx freebsd netbsd openbsd) "-f"]
    [else                            "-F"]))

;; Default values used for TTY initialization and manipulation.
(define current-default-tty-path        (make-parameter (path->string (cleanse-path "/dev/tty"))))
(define current-default-buffer-size     (make-parameter 8))
(define current-default-timeout-seconds (make-parameter 10))

;; The current TTY and its input mode.
(define current-tty                     (make-parameter #f))
(define current-input-mode              (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Low-Level TTY Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A [tty] consists of:
;;
;;   * The path to the TTY device, as a string. E.g., ["/dev/tty"].
;;   * The input port to that file.
;;   * The output port to that file.
;;   * A [bytes?] to be used as a buffer for reading/writing.
;;   * The size of the buffer.
;;   * The start position in the buffer from which to begin new reads.
;;   * The end position in the buffer beyond which no bytes should be read.
(struct tty (path                     ;; string
             [ input-port #:mutable]  ;; input-port?
             [output-port #:mutable]  ;; output-port?
             buffer                   ;; bytes?
             buffer-size              ;; positive-integer?
             [buffer-p0   #:mutable]  ;; nonnegative-integer?
             [buffer-pf   #:mutable]  ;; nonnegative-integer?
             original-settings))      ;; bytes?

;; Open a new TTY instance. If [raw?] is not [#f], the TTY is set to raw mode.
(define (open-tty #:path [path        (current-default-tty-path)]
                  #:size [buffer-size (current-default-buffer-size)]
                  #:raw? [raw?        #f])
  (with-handlers ([(λ _ #t)
                   (λ (e)
                     (error 'open-tty "failed to open tty ~a\n exception: ~a" path e))])
    (let*-values ([(in out)   (open-input-output-file path #:exists 'update)]
                  [(settings) (tty:get-settings #:tty-path path)]
                  [(tty)      (tty path in out (make-bytes buffer-size) buffer-size 0 0 settings)])
      (when raw? (tty:raw-mode! #:tty tty))
      tty)))

;; Close a TTY, closing its ports and resetting it to cooked input mode.
(define (close-tty [tty (current-tty)])
  (with-handlers ([(λ _ #t) (void)]) (close-input-port  (tty-input-port  tty)))
  (with-handlers ([(λ _ #t) (void)]) (close-output-port (tty-output-port tty)))
  (if (with-handlers ([(λ _ #t) (λ _ #f)])
        (stty* #:tty tty (tty-original-settings tty)))
      (if (eq? tty (current-tty))
          (current-tty #f)
          (void))
      (error 'close-tty "failed to close tty ~a" (tty-path tty))))

;; A convenience wrapper to handle set-up and tear-down of a TTY. Can specify a
;; path to use for the TTY device as well as the size of the buffer.
(define-syntax (with-tty stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:path path))
              (~optional (~seq #:size size))
              (~optional (~seq #:raw? raw?))) ...
        body ...+)
     #`(let ([new-tty #f])
         (dynamic-wind
           (λ ()
             (set! new-tty (open-tty #,@(or (and (attribute path)
                                                 (list #'#:path (attribute path)))
                                            (list))
                                     #,@(or (and (attribute size)
                                                 (list #'#:size (attribute size)))
                                            (list))
                                     #,@(or (and (attribute raw?)
                                                 (list #'#:raw? (attribute raw?)))
                                            (list)))))
           (λ ()
             (parameterize ([current-tty new-tty]
                            [current-input-mode 'original])
               body ...))
           (λ ()
             (close-tty new-tty)
             (set! new-tty #f))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; stty Interface
;;
;; The [stty] command-line utility is used for setting up and tearing down the
;; TTY, as well as any other investigations of the terminal's capabilities.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the [stty] command-line utility for the indicated TTY with the given
;; arguments. Uses default ports, e.g., [current-input-port] for input.
(define (stty* #:tty-path [path #f] #:tty [tty (current-tty)] . args)
  (apply system*
         stty-path
         stty-f-arg-string
         (or path (tty-path tty))
         args))

;; Like [stty*], but the output is captured into a byte string and returned.
(define (stty*/capture #:tty-path [path #f] #:tty [tty (current-tty)] . args)
  (parameterize ([current-output-port (open-output-bytes)]
                 [current-input-port  (open-input-bytes #"")]
                 [current-error-port  (open-output-bytes)])
    (unless (apply stty* #:tty-path path #:tty tty args)
      (error 'stty*/capture
             "stty process failed\n captured output:\n~a\ncaptured error output:\n~a"
             (get-output-bytes (current-output-port))
             (get-output-bytes (current-error-port))))
    (get-output-bytes (current-output-port))))

;; Gets the settings o the [tty], typically so they can be restored later.
(define (tty:get-settings #:tty-path [path #f] #:tty [tty (current-tty)] #:error? [error? #t])
  (or (stty*/capture #:tty-path path #:tty tty "-g")
      (and error?
           (error 'tty:get-settings "could not retrieve settings from stty"))))

;; Switch [tty] to raw input mode with echo disabled. This is useful for getting
;; input without needing the user to press [enter].
(define (tty:raw-mode! #:tty [tty (current-tty)] #:error? [error? #t])
  (or (stty* #:tty tty "raw" "-echo")
      (and error?
           (error 'tty:raw-mode! "could not switch to raw input mode"))))

;; Switch [tty] to cooked input mode with echo enabled. This is (typically) the
;; default mode of operation.
(define (tty:cooked-mode! #:tty [tty (current-tty)] #:error? [error? #t])
  (or (stty* #:tty tty "cooked" "echo")
      (and error?
           (error 'tty:cooked-mode! "could not initialize cooked input mode"))))

;; Restores [tty] to the settings that were recorded when it was opened.
(define (tty:original-mode! #:tty [tty (current-tty)] #:error? [error? #t])
  (or (stty* #:tty tty (tty-original-settings tty))
      (and error?
           (error 'tty:original-mode! "could not reset original settings"))))

;; Sets [tty] to the given [mode] of operation:
;;
;;   * ['raw]      -> raw input mode, echo disabled
;;   * ['cooked]   -> cooked input mode, echo enabled
;;   * ['original] -> restore the original settings, whatever they were
;;
;; If [error?] is [#f], this returns [#f] on failure. Otherwise, an error is
;; raised.
(define (tty:set-input-mode! #:tty    [tty (current-tty)]
                             #:error? [error? #t]
                             [mode    (current-input-mode)])
  (match mode
    ['raw      (tty:raw-mode!      #:tty tty #:error? error?)]
    ['cooked   (tty:cooked-mode!   #:tty tty #:error? error?)]
    ['original (tty:original-mode! #:tty tty #:error? error?)]
    [_ (error 'tty:set-input-mode! "unknown input mode: ~v" mode)]))

;; Executes the [body]s in raw input mode with echo disabled.
(define-syntax (with-raw-input stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(let ([old-input-mode (current-input-mode)])
         (begin0
             (parameterize ([current-input-mode 'raw])
               (tty:set-input-mode! #:tty (current-tty))
               body ...)
           (tty:set-input-mode! #:tty (current-tty) old-input-mode)))]))

;; Executes the [body]s in cooked input mode with echo enabled.
(define-syntax (with-cooked-input stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(let ([old-input-mode (current-input-mode)])
         (begin0
             (parameterize ([current-input-mode 'cooked])
               (tty:set-input-mode! #:tty tty)
               body ...)
           (tty:set-input-mode! #:tty tty old-input-mode)))]))

;; Executes the [body]s using the original settings, whatever they were.
(define-syntax (with-original-settings stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(let ([old-input-mode (current-input-mode)])
         (begin0
             (parameterize ([current-input-mode 'original])
               (tty:set-input-mode! #:tty tty)
               body ...)
           (tty:set-input-mode! #:tty tty old-input-mode)))]))

;; Uses [stty] to obtain the screen size. Returns two values: the number of rows
;; and the number of columns.
(define (tty:size #:tty [tty (current-tty)])
  (let ([text (stty*/capture #:tty tty "-a")])
    (match* {(regexp-match #px#"rows\\s+(\\d+)|\\s+(\\d+)\\s+rows" text)
             (regexp-match #px#"columns\\s+(\\d+)|\\s+(\\d+)\\s+columns" text)}
      [{(or (list _ #f rows-bs) (list _ rows-bs #f))
        (or (list _ #f cols-bs) (list _ cols-bs #f))}
       (values (string->number (bytes->string/utf-8 rows-bs))
               (string->number (bytes->string/utf-8 cols-bs)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Byte Manipulation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Moves the unread bytes (if any) to the beginning of the buffer and resets the
;; position indices.
(define (tty:shift-buffer #:tty [tty (current-tty)]
                          [minimum-remaining 10])
  (parameterize ([match-equality-test eq?])
    (match* {(tty-buffer-p0 tty) (tty-buffer-pf tty)}
      ;; If the start position is already at the beginning of the buffer, do
      ;; nothing.
      [{0  _ } (void)]
      ;; If the start position and end position are the same, reset them to the
      ;; beginning of the buffer.
      [{p  p } (set-tty-buffer-p0! tty 0)
               (set-tty-buffer-pf! tty 0)]
      ;; If the start position is more than [minimum-remaining] bytes from the
      ;; end of the buffer, do nothing.
      [{p0 pf} #:when (> (- (tty-buffer-size tty) p0)
                         minimum-remaining)
               (void)]
      ;; Otherwise, move the unread bytes to the beginning of the buffer and set
      ;; the positions accordingly.
      [{p0 pf} (let ([bs (tty-buffer tty)])
                 (bytes-copy! bs 0 bs p0 pf)
                 (set-tty-buffer-p0! tty 0)
                 (set-tty-buffer-pf! tty (- pf p0)))])))

;; Reads input from [tty] with a maximum timeout of [timeout] seconds, storing
;; the new bytes into the [tty-buffer]. Returns [#f] if nothing is read.
(define (tty:read-into-buffer/timeout #:tty [tty (current-tty)]
                                      [timeout (current-default-timeout-seconds)])
  (let ([in   (tty-input-port  tty)]
        [bs   (tty-buffer      tty)]
        [size (tty-buffer-size tty)]
        [pf   (tty-buffer-pf   tty)])
    (let loop ()
      (let ([sync-result (sync/timeout/enable-break timeout in)])
        (cond [(not sync-result) #f]
              [(eq? sync-result in)
               (let ([read-result (read-bytes-avail! bs in pf size)])
                 (if (zero? read-result)
                     (loop)
                     (begin
                       (set-tty-buffer-pf! tty (+ pf read-result))
                       read-result)))]
              [else (error 'read-into-buffer/timeout
                           "sync-result returned: ~v"
                           sync-result)])))))

;; Reads one byte from [tty], blocking indefinitely.
(define (tty:read-byte #:tty [tty (current-tty)])
  (tty:read-byte/timeout #f #:tty tty))

;; Reads one byte from [tty], timing out after [timeout] seconds.
(define (tty:read-byte/timeout #:tty [tty (current-tty)]
                               [timeout (current-default-timeout-seconds)])
  (let ([bs (tty-buffer    tty)]
        [p0 (tty-buffer-p0 tty)]
        [pf (tty-buffer-pf tty)])
    (and (or (< p0 pf)
             (tty:read-into-buffer/timeout timeout #:tty tty))
         (set-tty-buffer-p0! tty (add1 p0))
         (begin0
             (bytes-ref bs p0)
           (tty:shift-buffer)))))

;; Reads [n] bytes from [tty], blocking until completion.
(define (tty:read-bytes #:tty [tty (current-tty)] n)
  (let loop ([bs '()]
             [n n])
    (if (zero? n)
        (reverse bs)
        (loop (cons (tty:read-byte #:tty tty) bs) (sub1 n)))))

;; Reads [n] bytes from [tty], timing out after [timeout] seconds.
(define (tty:read-bytes/timeout #:tty [tty (current-tty)]
                                n
                                [timeout (current-default-timeout-seconds)])
  (let loop ([bs '()]
             [m 0]
             [timeout-remaining timeout])
    (if (>= m n)
        (values m (reverse bs))
        (let* ([t0 (current-seconds)]
               [b  (tty:read-byte/timeout #:tty tty timeout-remaining)]
               [tf (current-seconds)]
               [td (- tf t0)])
          (if b
              (loop (cons b bs)
                    (add1 m)
                    (- timeout-remaining td))
              (values m (reverse bs)))))))

;; Writes [b] to [tty].
(define (tty:write-byte #:tty [tty (current-tty)] b)
  (write-byte b (tty-output-port tty)))

;; Writes [bs] and, if present, [more-bs] to [tty].
(define (tty:write-bytes #:tty [tty (current-tty)] bs . more-bs)
  (let ([out (tty-output-port tty)]
        [bss (cons bs more-bs)])
    (for ([bs (in-list bss)])
      (write-bytes bs out))))

;; Writes the portion of [bs] from [p0] to [pf] to [tty].
(define (tty:write-subbytes #:tty [tty (current-tty)] bs p0 pf)
  (write-bytes bs (tty-output-port tty) p0 pf))
