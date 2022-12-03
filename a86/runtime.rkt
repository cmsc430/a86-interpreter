#lang racket

(require "memory.rkt"
         "utility.rkt"

         (for-syntax syntax/parse
                     racket/list))

(provide current-runtime
         reset-runtime
         ;; Parameters for runtime functions.
         runtime/flags
         runtime/registers
         runtime/memory
         runtime/stack-pointer
         ;; Using a runtime.
         runtime-has-func?
         runtime-ref
         ;; Defining runtimes.
         define/for-runtime
         undefine/for-runtime
         define-runtime
         define-runtimes
         ;; Runtimes.
         evildoer extort fraud hoax hustle iniquity jig knock loot
         hoodwink
         mountebank)

;; Runtimes are implemented as a simple opaque struct. This prevents users from
;; defining bad runtime values, perhaps by not properly converting their desired
;; functions. Instead of exporting this struct type, we provide a set of
;; functions to handle the creation/modification/deletion of runtimes.
(struct runtime (names->functions))

;; The six registers that can be used for passing arguments to subroutines. The
;; arguments are expected to be passed in the order of these arguments in this
;; list, i.e., the first argument should be passed in ['rdi] and so on.
(define argument-registers '(rdi rsi rdx rcx r8 r9))

;; The current runtime, represented as a hash mapping function names to Racket
;; functions. The functions are expected to have been converted via
;; [convert-runtime-function].
(define current-runtime (make-parameter (runtime (hash))))

;; These parameters will be set when the external function is called, giving the
;; foreign code access to everything.
(define runtime/flags (make-parameter #f))
(define runtime/registers (make-parameter #f))
(define runtime/memory (make-parameter #f))
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
(define (convert-runtime-function func)
  (let ([arity (procedure-arity func)])
    (unless (integer? arity)
      (raise-user-error 'initialize-state "a86 only supports runtime functions with fixed arity"))
    (let* ([reg-argc (min arity (length argument-registers))]
           [mem-argc (- arity reg-argc)])
      (λ (flags registers memory stack-pointer)
        (let* ([reg-args (for/list ([_ (in-range reg-argc)]
                                    [reg argument-registers])
                           (hash-ref registers reg))]
               [mem-args (for/list ([_ (in-range mem-argc)])
                           (begin0 (memory-ref memory stack-pointer)
                             (set! stack-pointer (lesser-word-aligned-address stack-pointer))))]
               [all-args (append reg-args mem-args)])
          (parameterize ([runtime/flags flags]
                         [runtime/registers registers]
                         [runtime/memory memory]
                         [runtime/stack-pointer stack-pointer])
            (apply func all-args)))))))

;; Resets the [current-runtime] hash.
(define (reset-runtime) (current-runtime (runtime (hash))))

;; Defines a function for use in the runtime.
;;
;; The function should be defined as a normal Racket function, and the arguments
;; will be filled in from the interpreter state when the function is called. The
;; registers and memory can be accessed via the provided parameters
;; [registers/runtime], [stack-memory/runtime], and [stack-pointer/runtime].
(define-syntax (define/for-runtime stx)
  (syntax-parse stx
    [(_ runtime (func-name args ...) body ...+)
     #'(hash-set! (runtime-names->functions runtime)
                  'func-name
                  (convert-runtime-function
                   (λ (args ...)
                     body ...)))]
    [(_ (func-name args ...) body ...+)
     #'(define/for-runtime (current-runtime)
         (func-name args ...) body ...)]))

;; Removes a function from use in the runtime, if it's defined. Does nothing
;; otherwise.
(define-syntax (undefine/for-runtime stx)
  (syntax-parse stx
    [(_ runtime func-name)
     #'(hash-remove! (runtime-names->functions runtime) 'func-name)]
    [(_ func-name)
     #'(undefine/for-runtime (current-runtime) func-name)]))

;; Whether the current runtime defines a function with the indicated name.
(define-syntax (runtime-has-func? stx)
  (syntax-parse stx
    [(_ runtime func-name)
     #'(hash-has-key? (runtime-names->functions runtime) func-name)]
    [(_ func-name)
     #'(runtime-has-func? (current-runtime) func-name)]))

;; Retrieves the function with the indicated name from the runtime.
(define-syntax (runtime-ref stx)
  (syntax-parse stx
    [(_ runtime func-name)
     #'(hash-ref (runtime-names->functions runtime) func-name)]
    [(_ func-name)
     #'(runtime-ref (current-runtime) func-name)]))

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
       #`(define-values (runtime-name ...)
           (values #,@runtimes)))]))

;; The various runtimes are defined below.
(define-runtimes (evildoer extort fraud hoax hustle iniquity jig knock loot)
  ([(read-byte)    (read-byte)]
   [(peek-byte)    (peek-byte)]
   [(write-byte b) (write-byte b)]))

(define-runtime hoodwink #:extending hoax
  ([(gensym) (gensym)]))

;; TODO: Implement these.
#;(define-runtime iniquity-gc #:extending iniquity
    ([(print-memory)    #f]
     [(collect_garbage) #f]
     [(alloc_val)       #f]))

;; TODO: Implement these.
(define-runtime mountebank #:extending loot
  ([(intern_symbol symb)  #f]
   [(symb_cmp s1 s2)      #f]
   [(memcpy dest src len) #f]))
