#lang racket

(provide test-specs run)

(require "../compile.rkt"
         "../parse.rkt"
         "../types.rkt"

         "../../../../a86/emulate.rkt"
         "../../../../a86/emulate/runtimes.rkt")

(current-runtime fraud)

(define test-specs
  `(["Abscond"   [ 7  7]
                 [-8 -8]]

    ["Blackmail" [9 (add1 (add1 7))]
                 [7 (add1 (sub1 7))]]

    ["Con"       [1 (if (zero? 0) 1 2)]
                 [2 (if (zero? 1) 1 2)]
                 [2 (if (zero? -7) 1 2)]
                 [2 (if (zero? 0)
                        (if (zero? 1) 1 2)
                        7)]
                 [7 (if (zero? (if (zero? 0) 1 0))
                        (if (zero? 1) 1 2)
                        7)]]

    ["Dupe"      [#t #t]
                 [#f #f]
                 [1  (if #t 1 2)]
                 [2  (if #f 1 2)]
                 [1  (if  0 1 2)]
                 [3  (if #t 3 4)]
                 [4  (if #f 3 4)]
                 [3  (if  0 3 4)]
                 [#f (zero? 4)]
                 [#t (zero? 0)]]

    ["Dodger"    [#\a #\a]
                 [#\b #\b]
                 [#t  (char? #\a)]
                 [#f  (char? #t)]
                 [#f  (char? 8)]
                 [#\λ (integer->char 955)]
                 [,(char->integer #\a) (char->integer #\a)]]

    ["Evildoer"  [,(void) (void)]
                 [2  (begin 1 2)]
                 [#f (eof-object? (void))]]

    ["Extort"    [err (add1 #f)]
                 [err (sub1 #f)]
                 [err (zero? #f)]
                 [err (char->integer #f)]
                 [err (integer->char #f)]
                 [err (integer->char -1)]
                 [err (write-byte #f)]
                 [err (write-byte -1)]
                 [err (write-byte 256)]
                 [#\b (begin (integer->char 97)
                             (integer->char 98))]]

    ["Fraud"     [7  (let ([x 7]) x)]
                 [2  (let ([x 7]) 2)]
                 [8  (let ([x 7]) (add1 x))]
                 [8  (let ([x (add1 7)]) x)]
                 [7  (let ([x 7]) (let ([y 2]) x))]
                 [2  (let ([x 7]) (let ([x 2]) x))]
                 [8  (let ([x 7]) (let ([x (add1 x)]) x))]
                 [7  (let ([x 0]) (if (zero? x) 7 8))]
                 [9  (let ([x 1]) (add1 (if (zero? x) 7 8)))]
                 [7  (+ 3 4)]
                 [-1 (- 3 4)]
                 [7  (+ (+ 2 1) 4)]
                 [7  (+ (+ 2 1) (+ 2 2))]
                 [7  (let ([x (+ 1 2)]) (let ([z (- 4 x)]) (+ (+ x x) z)))]
                 [#t (= 5 5)]
                 [#f (= 4 5)]
                 [#t (= (add1 4) 5)]
                 [#f (< 5 5)]
                 [#t (< 4 5)]
                 [#f (< (add1 4) 5)]]))

(define test-specs/io
  ;;               Out  Result    In   Program
  `(["Evildoer"  [(""   7)       (""   7)]
                 [("a"  ,(void)) (""   (write-byte 97))]
                 [(""   97)      ("a"  (read-byte))]
                 [("a"  98)      ("b"  (begin (write-byte 97)
                                              (read-byte)))]
                 [(""   ,eof)    (""   (read-byte))]
                 [(""   #t)      (""   (eof-object? (read-byte)))]
                 [(""   #f)      ("a"  (eof-object? (read-byte)))]
                 [("ab" ,(void)) (""   (begin (write-byte 97)
                                              (write-byte 98)))]
                 [(""   97)      ("ab" (peek-byte))]
                 [(""   97)      ("ab" (begin (peek-byte)
                                              (read-byte)))]
                 [(""   226)     ("†"  (read-byte))]
                 [(""   226)     ("†"  (peek-byte))]]

    ["Extort"    [(""   err)     (""   (write-byte #t))]]

    ["Fraud"     [("a"  ,(void)) (""   (let ([x 97]) (write-byte x)))]
                 [("a"  97)      (""   (let ([x 97]) (begin (write-byte x) x)))]
                 [(""   97)      ("b"  (let ([x 97]) (begin (read-byte) x)))]
                 [(""   97)      ("b"  (let ([x 97]) (begin (peek-byte) x)))]]))

(define (run e)
  (match (asm-emulate (compile (parse e)))
    ['err 'err]
    [bs (bits->value bs)]))

(define (run/io in e)
  (match (asm-emulate/io (compile (parse e)) in)
    [(cons 'err out) (cons 'err             out)]
    [(cons bs   out) (cons (bits->value bs) out)]))

(module+ test
  (require "../../test-specs.rkt")

  (run-test-specs "Fraud" test-specs run)

  (run-test-specs "Fraud / IO" test-specs/io run/io))
