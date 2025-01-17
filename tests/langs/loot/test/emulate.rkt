#lang racket

(provide test-specs run)

(require "../compile.rkt"
         "../parse.rkt"
         "../types.rkt"

         "../../../../a86/emulate.rkt"
         "../../../../a86/emulate/runtimes.rkt")

(define test-specs
  `(["Abscond"   [ 7 [ 7]]
                 [-8 [-8]]]

    ["Blackmail" [9 [(add1 (add1 7))]]
                 [7 [(add1 (sub1 7))]]]

    ["Con"       [1 [(if (zero? 0) 1 2)]]
                 [2 [(if (zero? 1) 1 2)]]
                 [2 [(if (zero? -7) 1 2)]]
                 [2 [(if (zero? 0)
                         (if (zero? 1) 1 2)
                         7)]]
                 [7 [(if (zero? (if (zero? 0) 1 0))
                         (if (zero? 1) 1 2)
                         7)]]]

    ["Dupe"      [#t [#t]]
                 [#f [#f]]
                 [1  [(if #t 1 2)]]
                 [2  [(if #f 1 2)]]
                 [1  [(if  0 1 2)]]
                 [3  [(if #t 3 4)]]
                 [4  [(if #f 3 4)]]
                 [3  [(if  0 3 4)]]
                 [#f [(zero? 4)]]
                 [#t [(zero? 0)]]]

    ["Dodger"    [#\a [#\a]]
                 [#\b [#\b]]
                 [#t  [(char? #\a)]]
                 [#f  [(char? #t)]]
                 [#f  [(char? 8)]]
                 [#\λ [(integer->char 955)]]
                 [97  [(char->integer #\a)]]]

    ["Evildoer"  [,(void) [(void)]]
                 [2       [(begin 1 2)]]
                 [#f      [(eof-object? (void))]]]

    ["Extort"    [err [(add1 #f)]]
                 [err [(sub1 #f)]]
                 [err [(zero? #f)]]
                 [err [(char->integer #f)]]
                 [err [(integer->char #f)]]
                 [err [(integer->char -1)]]
                 [err [(write-byte #f)]]
                 [err [(write-byte -1)]]
                 [err [(write-byte 256)]]
                 [#\b [(begin (integer->char 97)
                              (integer->char 98))]]]

    ["Fraud"     [7  [(let ([x 7]) x)]]
                 [2  [(let ([x 7]) 2)]]
                 [8  [(let ([x 7]) (add1 x))]]
                 [8  [(let ([x (add1 7)]) x)]]
                 [7  [(let ([x 7]) (let ([y 2]) x))]]
                 [2  [(let ([x 7]) (let ([x 2]) x))]]
                 [8  [(let ([x 7]) (let ([x (add1 x)]) x))]]
                 [7  [(let ([x 0]) (if (zero? x) 7 8))]]
                 [9  [(let ([x 1]) (add1 (if (zero? x) 7 8)))]]
                 [7  [(+ 3 4)]]
                 [-1 [(- 3 4)]]
                 [7  [(+ (+ 2 1) 4)]]
                 [7  [(+ (+ 2 1) (+ 2 2))]]
                 [7  [(let ([x (+ 1 2)]) (let ([z (- 4 x)]) (+ (+ x x) z)))]]
                 [#t [(= 5 5)]]
                 [#f [(= 4 5)]]
                 [#t [(= (add1 4) 5)]]
                 [#f [(< 5 5)]]
                 [#t [(< 4 5)]]
                 [#f [(< (add1 4) 5)]]]

    ["Hustle"    [()      ['()]]
                 [#t      [(empty? '())]]
                 [#f      [(empty? 3)]]
                 [#f      [(empty? (cons 1 2))]]
                 [#&1     [(box  1)]]
                 [#&-1    [(box -1)]]
                 [(1 . 2) [(cons 1 2)]]
                 [1       [(unbox (box 1))]]
                 [1       [(car (cons 1 2))]]
                 [2       [(cdr (cons 1 2))]]
                 [(1)     [(cons 1 '())]]
                 [1       [(let ([x (cons 1 2)])
                             (begin (cdr x)
                                    (car x)))]]
                 [3       [(let ([x (cons 1 2)])
                             (let ([y (box 3)])
                               (unbox y)))]]
                 [#t      [(eq? 1 1)]]
                 [#f      [(eq? 1 2)]]
                 [#f      [(eq? (cons 1 2) (cons 1 2))]]
                 [#t      [(let ([x (cons 1 2)])
                             (eq? x x))]]]

    ["Hoax"      [#()      [(make-vector 0 0)]]
                 [#(0)     [(make-vector 1 0)]]
                 [#(0 0 0) [(make-vector 3 0)]]
                 [#(5 5 5) [(make-vector 3 5)]]
                 [#t       [(vector? (make-vector 0 0))]]
                 [#f       [(vector? (cons 0 0))]]
                 [err      [(vector-ref (make-vector 0 #f) 0)]]
                 [err      [(vector-ref (make-vector 3 5) -1)]]
                 [5        [(vector-ref (make-vector 3 5) 0)]]
                 [5        [(vector-ref (make-vector 3 5) 1)]]
                 [5        [(vector-ref (make-vector 3 5) 2)]]
                 [err      [(vector-ref (make-vector 3 5) 3)]]
                 [#(4 5 5) [(let ([x (make-vector 3 5)])
                              (begin (vector-set! x 0 4)
                                     x))]]
                 [#(5 4 5) [(let ([x (make-vector 3 5)])
                              (begin (vector-set! x 1 4)
                                     x))]]
                 [3        [(vector-length (make-vector 3 #f))]]
                 [0        [(vector-length (make-vector 0 #f))]]
                 [""       [""]]
                 ["fred"   ["fred"]]
                 ["wilma"  ["wilma"]]
                 [""       [(make-string 0 #\f)]]
                 ["fff"    [(make-string 3 #\f)]]
                 ["ggg"    [(make-string 3 #\g)]]
                 [0        [(string-length "")]]
                 [4        [(string-length "fred")]]
                 [err      [(string-ref "" 0)]]
                 [err      [(string-ref (make-string 0 #\a) 0)]]
                 [#\f      [(string-ref "fred" 0)]]
                 [#\r      [(string-ref "fred" 1)]]
                 [#\e      [(string-ref "fred" 2)]]
                 [err      [(string-ref "fred" 4)]]
                 [#t       [(string? "fred")]]
                 [#f       [(string? (cons 1 2))]]
                 ["fff"    [(begin (make-string 3 #\f)
                                   (make-string 3 #\f))]]]

    ["Iniquity"  [5       [(define (f x) x)
                           (f 5)]]
                 [45      [(define (tri x)
                             (if (zero? x)
                                 0
                                 (+ x (tri (sub1 x)))))
                           (tri 9)]]
                 [#f      [(define (even? x)
                             (if (zero? x)
                                 #t
                                 (odd? (sub1 x))))
                           (define (odd? x)
                             (if (zero? x)
                                 #f
                                 (even? (sub1 x))))
                           (even? 101)]]
                 [(2 3 4) [(define (map-add1 xs)
                             (if (empty? xs)
                                 '()
                                 (cons (add1 (car xs))
                                       (map-add1 (cdr xs)))))
                           (map-add1 (cons 1 (cons 2 (cons 3 '()))))]]
                 [err     [(define (f x y) y)
                           (f 1 (add1 #f))]]]

    ;; TODO: Currently no Loot-specific tests.
    ["Loot"      ]))

(define test-specs/io
  ;;               Out  Result    In   Program
  `(["Evildoer"  [(""   7)       (""   [7])]
                 [("a"  ,(void)) (""   [(write-byte 97)])]
                 [(""   97)      ("a"  [(read-byte)])]
                 [("a"  98)      ("b"  [(begin (write-byte 97)
                                               (read-byte))])]
                 [(""   ,eof)    (""   [(read-byte)])]
                 [(""   #t)      (""   [(eof-object? (read-byte))])]
                 [(""   #f)      ("a"  [(eof-object? (read-byte))])]
                 [("ab" ,(void)) (""   [(begin (write-byte 97)
                                               (write-byte 98))])]
                 [(""   97)      ("ab" [(peek-byte)])]
                 [(""   97)      ("ab" [(begin (peek-byte)
                                               (read-byte))])]
                 [(""   226)     ("†"  [(read-byte)])]
                 [(""   226)     ("†"  [(peek-byte)])]]

    ["Extort"    [(""   err)     (""   [(write-byte #t)])]]

    ["Fraud"     [("a"  ,(void)) (""   [(let ([x 97]) (write-byte x))])]
                 [("a"  97)      (""   [(let ([x 97]) (begin (write-byte x) x))])]
                 [(""   97)      ("b"  [(let ([x 97]) (begin (read-byte) x))])]
                 [(""   97)      ("b"  [(let ([x 97]) (begin (peek-byte) x))])]]

    ["Iniquity"  [("abcdefghijklmnopqrstuvwxyz" ,(void))
                  ("" [(define (print-alphabet i)
                         (if (zero? i)
                             (void)
                             (begin (write-byte (- 123 i))
                                    (print-alphabet (sub1 i)))))
                       (print-alphabet 26)])]
                 [("a" ,(void))  (""   [(define (f x)
                                          (write-byte x))
                                        (f 97)])]
                 [("a" ,(void))  (""   [(define (f x y)
                                          (write-byte x))
                                        (f 97 98)])]
                 [("a" ,(void))  (""   [(define (f x)
                                          (let ((y x))
                                            (write-byte y)))
                                        (f 97)])]
                 [("a" ,(void))  (""   [(define (f x y)
                                          (let ((y x))
                                            (write-byte y)))
                                        (f 97 98)])]
                 [("a" ,(void))  (""   [(define (f x)
                                          (write-byte x))
                                        (let ((z 97))
                                          (f z))])]
                 [("a" ,(void))  (""   [(define (f x y)
                                          (write-byte x))
                                        (let ((z 97))
                                          (f z 98))])]]

    ;; TODO: Currently no Loot-specific tests.
    ["Loot"      ]))

(define (run e)
  (match (asm-emulate (compile (apply parse e)) loot)
    ['err 'err]
    [bs (bits->value bs)]))

(define (run/io in e)
  (match (asm-emulate/io (compile (apply parse e)) loot in)
    [(cons 'err out) (cons 'err             out)]
    [(cons bs   out) (cons (bits->value bs) out)]))

(module+ test
  (require "../../test-specs.rkt")

  (run-test-specs "Loot" test-specs run)

  (run-test-specs "Loot / IO" test-specs/io run/io))
