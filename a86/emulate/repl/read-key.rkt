;; This implementation of key-reading is inspired by
;; [charterm](https://docs.racket-lang.org/charterm/index.html), but has been
;; simplified to suit the lesser needs of this REPL.
#lang racket

(provide with-read-buffer
         shift-buffer
         buffer-read-byte
         buffer-read-byte/timeout
         buffer-write-byte
         buffer-write-bytes
         buffer-write-subbytes
         read-key
         read-key/timeout
         read-keys
         read-keys/timeout)

(require "tty.rkt"
         (for-syntax syntax/parse))

(define buffer-size 2048)

(struct read-buffer (input-port bytes [start-pos #:mutable] [end-pos #:mutable]))

(define current-read-buffer (make-parameter #f))

(define (make-read-buffer-from-tty)
  (read-buffer (current-tty-input-port)
               (make-bytes buffer-size)
               0
               0))

(define-syntax (with-read-buffer stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(with-tty
         (parameterize ([current-read-buffer (make-read-buffer-from-tty)])
           body ...))]))

;; Amount of time to wait in seconds before timing out.
(define default-timeout 10)

(define (shift-buffer [buffer (current-read-buffer)])
  (match buffer
    [(read-buffer _ _ p0 pf)
     (if (= p0 pf)
         (if (zero? pf)
             (void)
             (begin (set-read-buffer-start-pos! buffer 0)
                    (set-read-buffer-end-pos!   buffer 0)))
         (if (zero? p0)
             (void)
             (let ([bs (read-buffer-bytes buffer)])
               (bytes-copy! bs 0 bs p0 pf)
               (set-read-buffer-start-pos! buffer 0)
               (set-read-buffer-end-pos!   buffer (- pf p0)))))]))

(define (read-into-buffer/timeout [timeout default-timeout]
                                  #:buffer [buffer (current-read-buffer)])
  (match buffer
    [(read-buffer in bs _ pf)
     (let loop ()
       (let ([sync-result (sync/timeout/enable-break timeout in)])
         (cond [(not sync-result) #f]
               [(eq? sync-result in)
                (let ([read-result (read-bytes-avail! bs
                                                      in
                                                      pf
                                                      buffer-size)])
                  (if (zero? read-result)
                      (loop)
                      (begin
                        (set-read-buffer-end-pos! buffer (+ pf read-result))
                        read-result)))]
               [else (error 'read-into-buffer/timeout
                            "sync-result returned: ~v"
                            sync-result)])))]))

(define (buffer-read-byte #:buffer [buffer (current-read-buffer)])
  (buffer-read-byte/timeout #f #:buffer buffer))

(define (buffer-read-byte/timeout [timeout default-timeout]
                                  #:buffer [buffer (current-read-buffer)])
  (match buffer
    [(read-buffer _ bs p0 pf)
     (if (or (< p0 pf)
             (read-into-buffer/timeout timeout #:buffer buffer))
         (begin0
             (bytes-ref bs p0)
           (set-read-buffer-start-pos! buffer (add1 p0)))
         #f)]))

(define (buffer-write-byte b)
  (write-byte b (current-tty-output-port)))

(define (buffer-write-bytes bs . more-bs)
  (for ([bs (in-list (cons bs more-bs))])
    (write-bytes bs (current-tty-output-port))))

(define (buffer-write-subbytes bs p0 pf)
  (write-bytes bs (current-tty-output-port) p0 pf))

(define (read-key)
  (integer->char (buffer-read-byte)))

(define (read-keys n)
  (let loop ([keys '()])
    (if (>= n (length keys))
        (reverse keys)
        (loop (cons (read-key) keys)))))

(define (read-key/timeout [timeout default-timeout])
  (let ([b (buffer-read-byte/timeout timeout)])
    (and b
         (integer->char b))))

(define (read-keys/timeout n [timeout default-timeout])
  (let loop ([keys '()]
             [timeout-remaining timeout])
    (let* ([t0 (current-seconds)]
           [k  (read-key/timeout timeout-remaining)]
           [tf (current-seconds)]
           [td (- tf t0)])
      (if k
          (loop (cons k keys)
                (- timeout-remaining td))
          (values (length keys)
                  (reverse keys))))))
