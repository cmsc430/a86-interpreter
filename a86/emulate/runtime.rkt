#lang racket

(require "../registers.rkt"
         "../utility.rkt"

         "etypes.rkt"
         "memory.rkt"

         (for-syntax syntax/parse
                     racket/list))

(provide (contract-out
          [runtime?                    (-> any/c boolean?)]
          [current-runtime             (parameter/c runtime?)]
          [current-runtime-input-port  (parameter/c (or/c #f input-port?))]
          [current-runtime-output-port (parameter/c (or/c #f output-port?))]
          [reset-runtime               (-> void?)]
          ;; Parameters for runtime functions.
          [runtime/flags               (parameter/c flags?)]
          [runtime/registers           (parameter/c registers?)]
          [runtime/memory              (parameter/c memory?)]
          [runtime/stack-pointer       (parameter/c a86-value?)]
          ;; Using a runtime.
          [runtime-has-func?           (case-> (->          symbol? boolean?)
                                               (-> runtime? symbol? boolean?))]
          ;; TODO: The [procedure?] checks here could maybe be constrained to be
          ;; some kind of [runtime-converted-procedure?] to verify they work as
          ;; runtime functions.
          [runtime-ref                 (case-> (->          symbol? procedure?)
                                               (-> runtime? symbol? procedure?))]
          [runtime-funcs               (case-> (->          (listof symbol?))
                                               (-> runtime? (listof symbol?)))]
          ;; Finding defined runtimes.
          [runtime-name?               (-> symbol? boolean?)]
          [name->runtime               (-> symbol? runtime?)]
          ;; Predefined runtimes.
          [evildoer                    runtime?]
          [extort                      runtime?]
          [fraud                       runtime?]
          [hoax                        runtime?]
          [hustle                      runtime?]
          [iniquity                    runtime?]
          [jig                         runtime?]
          [knock                       runtime?]
          [loot                        runtime?]
          [hoodwink                    runtime?])
         ;; Defining runtimes.
         define-runtime
         define-runtimes
         define/for-runtime
         undefine/for-runtime)

;; Runtimes are implemented as a simple opaque struct. This prevents users from
;; defining bad runtime values, perhaps by not properly converting their desired
;; functions. Instead of exporting this struct type, we provide a set of
;; functions to handle the creation/modification/deletion of runtimes.
;;
;; TODO: Add custom printing to show currently defined functions.
(struct runtime ([names->functions #:mutable]))

;; The six registers that can be used for passing arguments to subroutines. The
;; arguments are expected to be passed in the order of these arguments in this
;; list, i.e., the first argument should be passed in ['rdi] and so on.
(define argument-registers '(rdi rsi rdx rcx r8 r9))

;; The current runtime, represented as a hash mapping function names to Racket
;; functions. The functions are expected to have been converted via
;; [convert-runtime-function].
(define current-runtime (make-parameter (runtime (hash))))

;; I/O is handled through dedicated ports.
(define current-runtime-input-port  (make-parameter #f))
(define current-runtime-output-port (make-parameter #f))

;; These parameters will be set when the external function is called, giving the
;; foreign code access to everything.
(define runtime/flags         (make-parameter #f))
(define runtime/registers     (make-parameter #f))
(define runtime/memory        (make-parameter #f))
(define runtime/stack-pointer (make-parameter #f))

;; Converts an external runtime function into a function that can be used with
;; our machine.
;;
;; The first six arguments to the function are read from the following registers
;; in the order defined by [argument-registers]. Any additional arguments are
;; assumed to have been pushed onto the stack in reverse order, i.e., the last
;; argument should be the one that was pushed onto the stack first, and the
;; seventh argument should be the one that was pushed onto the stack last before
;; calling the function.
;;
;; The called function must return a single integer result, which will be stored
;; in the 'rax register.
;;
;; TODO: Generalize this so the functionality can be customized more easily.
(define (convert-runtime-function func)
  (let ([arity (procedure-arity func)])
    (unless (integer? arity)
      (raise-user-error 'initialize-state "a86 only supports runtime functions with fixed arity"))
    (let* ([reg-argc (min arity (length argument-registers))]
           [mem-argc (- arity reg-argc)])
      (λ (flags registers memory stack-pointer)
        (let* ([reg-args (for/list ([_ (in-range reg-argc)]
                                    [reg argument-registers])
                           (register-ref registers reg))]
               [mem-args (for/list ([_ (in-range mem-argc)])
                           (begin0 (memory-ref memory stack-pointer)
                             (set! stack-pointer (lesser-word-aligned-address stack-pointer))))]
               [all-args (append reg-args mem-args)])
          (parameterize ([runtime/flags         flags]
                         [runtime/registers     registers]
                         [runtime/memory        memory]
                         [runtime/stack-pointer stack-pointer])
            (apply func all-args)))))))

;; Resets the [current-runtime] hash.
(define (reset-runtime) (current-runtime (runtime (hash))))

;; Whether the current runtime defines a function with the indicated name.
(define runtime-has-func?
  (case-lambda
    [(func-name)
     (runtime-has-func? (current-runtime) func-name)]
    [(runtime func-name)
     (hash-has-key? (runtime-names->functions runtime) func-name)]))

;; Retrieves the function with the indicated name from the runtime.
(define runtime-ref
  (case-lambda
    [(func-name)
     (runtime-ref (current-runtime) func-name)]
    [(runtime func-name)
     (hash-ref (runtime-names->functions runtime) func-name)]))

;; Returns a list of the functions currently defined in the runtime.
(define runtime-funcs
  (case-lambda
    [()
     (runtime-funcs (current-runtime))]
    [(runtime)
     (hash-keys (runtime-names->functions runtime))]))

;; An association list mapping runtime names to the [runtime?]s.
(define predefined-runtimes (make-hash))
(define (runtime-name? name) (hash-has-key? predefined-runtimes name))
(define (name->runtime name) (hash-ref predefined-runtimes name #f))

;; Defines a runtime.
(define-syntax (define-runtime stx)
  (syntax-parse stx
    [(_ runtime-name (~optional (~seq #:extending other-runtime)) bindings)
     #`(define-runtimes (runtime-name) #,@(if (attribute other-runtime)
                                              (syntax->list #'(#:extending other-runtime))
                                              (list)) bindings)]))

;; Defines multiple runtime names with the same contents, allocating different
;; [runtime?] objects for each.
(define-syntax (define-runtimes stx)
  (syntax-parse stx
    [(_ names (~seq #:extending other-runtime-name:id) bindings)
     #'(define-runtimes names
         #:extending (runtime-names->functions other-runtime-name)
         bindings)]
    [(_ (runtime-name:id ...) (~optional (~seq #:extending other-runtime))
        ([(func-name:id arg:id ...) body ...+] ...))
     (let* ([names (syntax->list #'('func-name ...))]
            [funcs (syntax->list #'((convert-runtime-function
                                     (λ (arg ...) body ...)) ...))]
            [runtime-def #`(runtime (#,@(if (attribute other-runtime)
                                            (syntax->list #'(hash-set* other-runtime))
                                            (list #'hash))
                                     #,@(flatten (map list names funcs))))]
            [runtimes (make-list (length (attribute runtime-name)) runtime-def)])
       #`(begin (define-values (runtime-name ...)
                  (values #,@runtimes))
                (hash-set*! predefined-runtimes
                            #,@(flatten (map list
                                             (syntax->list #'('runtime-name ...))
                                             (syntax->list #'(runtime-name ...)))))))]))

;; Defines a function for use in the runtime.
;;
;; The function should be defined as a normal Racket function, and the arguments
;; will be filled in from the interpreter state when the function is called. The
;; registers and memory can be accessed via the provided parameters
;; [registers/runtime], [stack-memory/runtime], and [stack-pointer/runtime].
(define-syntax (define/for-runtime stx)
  (syntax-parse stx
    [(_ (~seq #:runtime runtime) (func-name args ...) body ...+)
     #'(set-runtime-names->functions!
        runtime
        (hash-set (runtime-names->functions runtime)
                  'func-name
                  (convert-runtime-function
                   (λ (args ...)
                     body ...))))]
    [(_ (func-name args ...) body ...+) ;; TODO: Use [function-header] syntax class?
     #'(define/for-runtime #:runtime (current-runtime)
         (func-name args ...) body ...)]))

;; Removes a function from use in the runtime, if it's defined. Does nothing
;; otherwise.
(define-syntax (undefine/for-runtime stx) ;; FIXME: Broken!
  (syntax-parse stx
    [(_ runtime func-name)
     #'(hash-remove! (runtime-names->functions runtime) 'func-name)]
    [(_ func-name)
     #'(undefine/for-runtime (current-runtime) func-name)]))

(define-syntax (define/for-runtime/io stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        (~seq #:mode (~or* (~and (~datum i)
                                 (~bind [uses-input? #t] [uses-output? #f]))
                           (~and (~datum o)
                                 (~bind [uses-input? #f] [uses-output? #f]))
                           (~and (~or* (~datum io) (~datum oi))
                                 (~bind [uses-input? #t] [uses-output? #t]))) ...)
        body:expr ...+)
     #`(define (name arg ...)
         #,@(if (attribute uses-input?)
                (list #'(unless (current-runtime-input-port)
                          (raise-user-error
                           'name
                           "current-runtime-input-port not parameterized; did you mean to run in I/O mode?")))
                (list))
         #,@(if (attribute uses-output?)
                (list #'(unless (current-runtime-output-port)
                          (raise-user-error
                           'name
                           "current-runtime-output-port not parameterized; did you mean to run in I/O mode?")))
                (list))
         (parameterize (#,@(if (attribute uses-input?)
                               (list #'[current-input-port (current-runtime-input-port)])
                               (list))
                        #,@(if (attribute uses-output?)
                               (list #'[current-output-port (current-runtime-output-port)])
                               (list)))
           body ...))]))

(define-syntax (define/for-runtime/io* stx)
  (syntax-parse stx
    [(_ [(name:id arg:id ...) mode [body:expr ...+]] ...)
     #'(begin (define/for-runtime/io (name arg ...) #:mode mode body ...) ...)]))

(define/for-runtime/io*
  [(guarded-read-byte)    i [(let ([b (read-byte)])
                               (if (eof-object? b)
                                   (truncate-integer/signed -1)
                                   (convert b uchar)))]]
  [(guarded-peek-byte)    i [(let ([b (peek-byte)])
                               (if (eof-object? b)
                                   (truncate-integer/signed -1)
                                   (convert b uchar)))]]
  [(guarded-write-byte b) o [(write-byte (convert b char))]])


;; C standard library implementation.
;;
;;   malloc
;;   free
;;   printf
;;   exit
;;   putchar
;;   getc
;;   ungetc
;;   putc
;;   sizeof
;;   memcpy

#;(define-runtime libc
  ([(malloc size) ()]))


;; The various runtimes are defined below.
(define-runtime evildoer
  ([(read_byte)    (guarded-read-byte)]
   [(peek_byte)    (guarded-peek-byte)]
   [(write_byte b) (guarded-write-byte b)]))

(define-runtimes (extort fraud hoax hustle iniquity jig knock loot)
  #:extending evildoer
  ([(raise_error) (raise 'err)]))

(define-runtime hoodwink #:extending hoax
  ([(gensym) (gensym)]))

;; TODO: Implement these.
#;(define-runtime iniquity-gc #:extending iniquity
    ([(print-memory)    #f]
     [(collect_garbage) #f]
     [(alloc_val)       #f]))

;; TODO: Implement these.
#;(define-runtime mountebank #:extending loot
  ([(intern_symbol symb)  #f]
   [(symb_cmp s1 s2)      #f]
   [(memcpy dest src len) #f]))
