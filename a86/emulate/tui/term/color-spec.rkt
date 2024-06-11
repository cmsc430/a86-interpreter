#lang racket

(provide mode-names
         color-names
         make-color-spec
         current-color-spec
         color-spec->escape-code

         define-color-spec
         define-color-specs
         define/provide-color-specs)

(require (for-syntax racket/string
                     syntax/parse))

(define modes
  (hash 'none          #f  ;; NOTE: Be careful with this!
        'reset         0
        'bold          1
        'dim           2
        'italic        3
        'underline     4
        'blink         5
        ;; 'fast-blink    6   ;; NOTE: Disabled because it's not supported on many
        ;;                    ;; terminals.
        'inverse       7
        'invisible     8
        'strikethrough 9))
(define mode-names (hash-keys modes))
(define (mode? x) (hash-has-key? modes x))

(define colors
  (hash 'black   0
        'red     1
        'green   2
        'yellow  3
        'blue    4
        'magenta 5
        'cyan    6
        'white   7
        'default 9))
(define color-names (hash-keys colors))
(define (color? x) (hash-has-key? colors x))

;; 16-bit foreground and background colors are given relative to these bases.
(define foreground-base 30)
(define background-base 40)

;; A struct is used to shuttle around specifications.
(struct color-spec (mode fg bg) #:prefab)

;; A [color-spec] can be converted into a non-printing escape code to coerce
;; the terminal into changing the display colors.
(define color-spec->escape-code
  (match-lambda
    [(color-spec mode fg bg)
     (let ([mode-code (hash-ref modes mode)]
           [fg-code (+ foreground-base (hash-ref colors fg))]
           [bg-code (+ background-base (hash-ref colors bg))])
       (string-append
        "\x1b["
        (if mode-code
            (~a #:separator ";" mode-code fg-code bg-code)
            (~a #:separator ";"           fg-code bg-code))
        "m"))]))

;; Holes in specifications are filled with these default values.
(define default-mode 'none)
(define default-fg   'default)
(define default-bg   'default)

;; Creates a [color-spec] from a symbolic representation. This allows for
;; overriding the default mode, foreground color, and background color in case
;; the given specification is missing any of these.
;;
;; In addition to the obvious form of [(make-color-spec 'mode 'fg 'bg)], this
;; function supports optionally omitting pieces of the specification to be
;; filled with the default values. Valid specifications obey the following
;; mapping:
;;
;;   Specification                  Mapping
;;
;;     ()                       =>    (default-mode  default-fg  default-bg)
;;     (m)                      =>    (m             default-fg  default-bg)
;;     (c)                      =>    (default-mode  c           default-bg)
;;     ('background c)          =>    (default-mode  default-fg  c         )
;;     ('on c)                  =>    (default-mode  default-fg  c         )
;;     (m c)                    =>    (m             c           default-bg)
;;     (m 'background c)        =>    (m             default-fg  c         )
;;     (m 'on c)                =>    (m             default-fg  c         )
;;     (c1 c2)                  =>    (default-mode  c1          c2        )
;;     (c1 'background c2)      =>    (default-mode  c1          c2        )
;;     (c1 'on c2)              =>    (default-mode  c1          c2        )
;;     (m c1 c2)                =>    (m             c1          c2        )
;;     (m c1 'background c2)    =>    (m             c1          c2        )
;;     (m c1 'on c2)            =>    (m             c1          c2        )
(define (make-color-spec #:default-mode   [default-mode   default-mode]
                         #:default-fg     [default-fg     default-fg]
                         #:default-bg     [default-bg     default-bg]
                         #:failure-result [failure-result
                                           (λ (spec)
                                             (error 'make-color-spec
                                                    "invalid color spec: ~a"
                                                    spec))]
                         . spec)
  (match spec
    ;; Special case for existing [color-spec].
    [(list (? color-spec?))
     spec]
    ;; 0-argument spec.
    [(list)
     (color-spec default-mode default-fg default-bg)]
    ;; 1-argument spec.
    [(list (? mode? mode))
     (color-spec mode default-fg default-bg)]
    [(list (? color? fg))
     (color-spec default-mode fg default-bg)]
    [(list (or 'background 'on) (? color? bg))
     (color-spec default-mode default-fg bg)]
    ;; 2-argument spec.
    [(list (? mode? mode) (? color? fg))
     (color-spec mode fg default-bg)]
    [(list (? mode? mode) (or 'background 'on) (? color? bg))
     (color-spec mode default-fg bg)]
    [(or (list (? color? fg) (? color? bg))
         (list (? color? fg) (or 'background 'on) (? color? bg)))
     (color-spec default-mode fg bg)]
    ;; 3-argument spec.
    [(or (list (? mode? mode) (? color? fg) (? color? bg))
         (list (? mode? mode) (? color? fg) (or 'background 'on) (? color? bg)))
     (color-spec mode fg bg)]
    ;; Erroneous specification.
    [_ (failure-result spec)]))

(define current-color-spec
  (make-parameter
   (make-color-spec)
   (λ (spec)
     (match (current-color-spec)
       [(color-spec old-mode old-fg old-bg)
        (apply
         make-color-spec
         spec
         #:default-mode old-mode
         #:default-fg   old-fg
         #:default-bg   old-bg)]))
   'current-color-spec))

(define-syntax (define-color-spec stx)
  (define-syntax-class hyphen-separated
    (pattern n:id
             #:with (parts ...)
             (map (λ (p) #`'#,p)
                  (map string->symbol
                       (string-split (symbol->string (syntax-e #'n))
                                     "-")))))
  (syntax-parse stx
    [(_ color-spec-name:hyphen-separated)
     #'(define color-spec-name (make-color-spec color-spec-name.parts ...))]))

(define-syntax define-color-specs
  (syntax-rules ()
    [(define-color-specs spec-name ...)
     (begin (define-color-spec spec-name) ...)]))

(define-syntax define/provide-color-specs
  (syntax-rules ()
    [(define/provide-color-specs spec-name ...)
     (begin
       (provide spec-name ...)
       (define-color-specs spec-name ...))]))

(define/provide-color-specs
  bold dim underline blink inverse

  black             background-black
  red               background-red
  green             background-green
  yellow            background-yellow
  blue              background-blue
  magenta           background-magenta
  cyan              background-cyan
  white             background-white

  bold-black        bold-background-black
  bold-red          bold-background-red
  bold-green        bold-background-green
  bold-yellow       bold-background-yellow
  bold-blue         bold-background-blue
  bold-magenta      bold-background-magenta
  bold-cyan         bold-background-cyan
  bold-white        bold-background-white

  dim-black         dim-background-black
  dim-red           dim-background-red
  dim-green         dim-background-green
  dim-yellow        dim-background-yellow
  dim-blue          dim-background-blue
  dim-magenta       dim-background-magenta
  dim-cyan          dim-background-cyan
  dim-white         dim-background-white

  red-on-black      bold-red-on-black
  green-on-black    bold-green-on-black
  yellow-on-black   bold-yellow-on-black
  blue-on-black     bold-blue-on-black
  magenta-on-black  bold-magenta-on-black
  cyan-on-black     bold-cyan-on-black
  white-on-black    bold-white-on-black)
