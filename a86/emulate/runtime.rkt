#lang racket

(require "../registers.rkt"
         "../utility.rkt"

         "memory.rkt"

         (only-in racket/private/hash
                  paired-fold)

         (for-syntax syntax/parse

                     (submod "../utility.rkt" racket/syntax)))

(provide (contract-out
          ;; Basic runtime features.
          [runtime?                        (-> any/c boolean?)]
          [empty-runtime                   runtime?]
          [current-runtime                 (parameter/c runtime?)]
          [reset-runtime                   (-> void?)]
          ;; Features supporting external functions.
          [argument-registers              (listof register?)]
          [volatile-registers              (listof register?)]
          [non-volatile-registers          (listof register?)]
          ;; Parameters for runtime functions.
          [runtime/stack-pointer           (parameter/c a86-value?)]
          [runtime/flags                   (parameter/c flags?)]
          [runtime/registers               (parameter/c registers?)]
          [runtime/memory                  (parameter/c memory?)]
          [runtime/flag-ref                (parameter/c (-> flag? boolean?))]
          [runtime/register-ref            (parameter/c (-> registers? register? a86-value?))]
          [runtime/register-set!           (parameter/c (-> register? a86-value? void?))]
          [runtime/register-set*!          (parameter/c (-> any/c ... void?))]
          [runtime/register-set/truncate!  (parameter/c (-> register? a86-value? void?))]
          [runtime/register-set*/truncate! (parameter/c (-> any/c ... void?))]
          [runtime/memory-ref              (parameter/c (-> address? any/c))]
          [runtime/memory-set!             (parameter/c (->* [address? a86-value?]
                                                             [positive-integer?]
                                                             any/c))]
          [current-runtime-input-port      (parameter/c (or/c #f input-port?))]
          [current-runtime-output-port     (parameter/c (or/c #f output-port?))]
          [runtime-function?               (-> any/c boolean?)]
          [call-runtime-function           (->
                                            ;; func
                                            runtime-function?
                                            ;; stack-pointer flags  registers  memory
                                            a86-value?       flags? registers? memory?
                                            ;; flag-ref
                                            (-> flag? boolean?)
                                            ;; register-ref
                                            (-> registers? register? a86-value?)
                                            ;; register-set
                                            (-> registers? register? a86-value? registers?)
                                            ;; register-set/truncate
                                            (-> registers? register? a86-value?)
                                            ;; memory-ref
                                            (-> address? any/c)
                                            ;; memory-set!
                                            (->* [address? a86-value?]
                                                 [positive-integer?]
                                                 any/c)
                                            ;; return values
                                            (values any/c registers?))]
          ;; Using a runtime.
          [runtime-has-function?           (case-> (-> runtime? symbol? boolean?)
                                                   (->          symbol? boolean?))]
          [runtime-function-names          (case-> (-> runtime? (listof symbol?))
                                                   (->          (listof symbol?)))]
          [runtime-function-ref            (case-> (-> runtime? symbol? runtime-function?)
                                                   (->          symbol? runtime-function?))]
          ;; Finding defined runtimes.
          [runtime-exists?                 (-> symbol? boolean?)]
          [name->runtime                   (-> symbol? (or/c #f runtime?))])
         ;; Defining runtimes.
         define-runtimes)

;; A runtime is implemented as a simple opaque struct. This prevents users from
;; defining bad runtime values, perhaps by not properly converting their desired
;; functions. Instead of directly exporting this struct type, we provide a set
;; of functions to handle the creation/modification/deletion of runtimes.
;;
;; A runtime is composed of:
;;
;;   1. A [bits->value] function that converts bitwise representations to Racket
;;      values. The default value is the identity function.
;;   2. A [describe-bits] function that converts bitwise representations to
;;      descriptions of those values' purposes. The default action is to call
;;      the [bits->value] function.
;;   3. A [value->bits] function that converts Racket values to bitwise
;;      representations. The default value is the identity function.
;;   4. A collection of functions. The default value is an empty [hash?].
;;
;; TODO: Add custom printing to show currently defined functions.
(struct runtime
  (bits->value
   describe-bits
   value->bits
   external-functions)
  #:transparent)

;; The empty runtime has nothing defined.
(define empty-runtime
  (runtime
   (λ (b) (error 'bits->value "invalid bits: ~v" b))
   (λ (b) (error 'describe-bits "cannot describe bits: ~v" b))
   (λ (v) (error 'value->bits "cannot encode value: ~v" v))
   (hash)))

;; The current runtime.
(define current-runtime (make-parameter empty-runtime))

;; Resets the [current-runtime].
(define (reset-runtime) (current-runtime empty-runtime))

;; The six registers that can be used for passing arguments to subroutines. The
;; arguments are expected to be passed in the order of these arguments in this
;; list, i.e., the first argument should be passed in ['rdi] and so on.
(define argument-registers     '(rdi rsi rdx rcx r8  r9))

;; We follow the System V ABI, and we enforce volatile/non-volatile
;; expectations. If an external function call returns and any of the
;; non-volatile registers has been modified, and error will be raised at
;; run-time. Prior to returning control, the volatile registers will have random
;; values inserted, simulating use regardless of what the function does.
;;
;; TODO: Make this behavior easily customizable, e.g., via a toggle.
(define volatile-registers     '(rax rdi rsi rdx rcx r8  r9  r10 r11))
(define non-volatile-registers '(rbx rsp rbp r12 r13 r14 r15))

;; These parameters will be set when an external function is called, giving the
;; foreign code access to everything possibly necessary.
(define runtime/stack-pointer           (make-parameter #f))
(define runtime/flags                   (make-parameter #f))
(define runtime/registers               (make-parameter #f))
(define runtime/memory                  (make-parameter #f))
(define runtime/flag-ref                (make-parameter #f))
(define runtime/register-ref            (make-parameter #f))
(define runtime/register-set!           (make-parameter #f))
(define runtime/register-set*!          (make-parameter #f))
(define runtime/register-set/truncate!  (make-parameter #f))
(define runtime/register-set*/truncate! (make-parameter #f))
(define runtime/memory-ref              (make-parameter #f))
(define runtime/memory-set!             (make-parameter #f))

;; I/O is handled through dedicated ports.
(define current-runtime-input-port  (make-parameter #f))
(define current-runtime-output-port (make-parameter #f))

;; If an external function is called that is not marked as [#:uses-input], the
;; [current-input-port] will be set to a custom disabled input port customized
;; to the name of the function.
(define (make-disabled-input-port who-sym)
  (make-input-port (string->symbol (format "disabled-input-port:~s" who-sym))
                   ;; read-in
                   (λ _ (raise-user-error who-sym "runtime function is not allowed to consume input"))
                   ;; peek
                   #f
                   ;; close
                   (lambda _
                     (displayln (format "closing disabled-input-port:~s" who-sym))
                     void)
                   ;; [get-progress-evt #f]
                   ;; [commit           #f]
                   ;; [get-location     #f]
                   ;; [count-lines!     #f]
                   ;; [init-position     1]
                   ;; [buffer-mode      #f]
                   ))

;; If an external function is called that is not marked as [#:uses-output], the
;; [current-output-port] will be set to a custom disabled output port customized
;; to the name of the function.
(define (make-disabled-output-port who-sym)
  (make-output-port (string->symbol (format "disabled-output-port:~s" who-sym))
                    ;; evt
                    always-evt
                    ;; write-out
                    (λ _ (raise-user-error who-sym "runtime function is not allowed to produce output"))
                    ;; close
                    (lambda _
                      (displayln (format "closing disabled-output-port:~s" who-sym))
                      void)
                    ;; [write-out-special     #f]
                    ;; [get-write-evt         #f]
                    ;; [get-write-special-evt #f]
                    ;; [get-location          #f]
                    ;; [count-lines!          void]
                    ;; [init-position         1]
                    ;; [buffer-mode           #f]
                    ))

;; External functions consist of:
;;
;;   1. A [name], given as a symbol. This is used in reporting errors.
;;   2. An [arity], which is used to facilitate argument-getting during a call.
;;   3. A [procedure], which is a Racket procedure encapsulating the desired
;;      functionality.
;;   4. A [convert-argument] procedure, which is called on each argument that is
;;      going to be passed to the [procedure] during the call.
;;   5. A [convert-result] procedure, which is called on the result prior to
;;      returning from the call.
;;   6. A Boolean value [uses-input?], indicating whether the function requires
;;      the [current-runtime-input-port] to be set.
;;   7. A Boolean value [uses-output?], indicating whether the function requires
;;      the [current-runtime-output-port] to be set.
(struct runtime-function (name
                          arity
                          procedure
                          convert-argument
                          convert-result
                          uses-input?
                          uses-output?)
  #:transparent)

;; Calling an external runtime function is a tricky business. This function
;; should be used by [step] to properly guard the function call boundary.
;; Calling a function in this manner does all of the following:
;;
;;   - Checks input/output port requirements.
;;   - Gets all the arguments needed, based on the function's arity.
;;   - Converts the arguments with the function's [convert-argument] procedure.
;;   - Records the values of the non-volatile registers.
;;   - Parameterizes the various [runtime/] capabilities (documented above).
;;   - Parameterizes the [current-input-port] and [current-output-port],
;;     the values of which depend on whether the function was declared as using
;;     input or output, respectively.
;;   - Applies the function's [procedure] and keeps track of the registers along
;;     the way.
;;   - Checks that the non-volatile registers maintained their original values.
;;   - Randomizes the values in the volatile registers. NOTE: Includes [rax]!
;;   - Converts the result of the [procedure] with [convert-result].
;;   - Returns both the converted result and the updated registers.
(define (call-runtime-function func
                               stack-pointer flags registers memory
                               flag-ref
                               register-ref register-set register-set/truncate
                               memory-ref memory-set!)
  (match func
    [(runtime-function name arity procedure convert-argument convert-result uses-input? uses-output?)
     (when (and uses-input?
                (not (current-runtime-input-port)))
       (raise-user-error name
                         "current-runtime-input-port not parameterized; did you mean to run in I/O mode?"))
     (when (and uses-output?
                (not (current-runtime-output-port)))
       (raise-user-error name
                         "current-runtime-output-port not parameterized; did you mean to run in I/O mode?"))
     (let* ([reg-argc (min arity (length argument-registers))]
            [mem-argc (- arity reg-argc)]
            [reg-args (for/list ([_ (in-range reg-argc)]
                                 [reg argument-registers])
                        (register-ref registers reg))]
            [mem-args (for/list ([_ (in-range mem-argc)])
                        (begin0 (memory-ref stack-pointer)
                          (set! stack-pointer (lesser-word-aligned-address stack-pointer))))]
            [all-args (map convert-argument (append reg-args mem-args))]
            [preserved-values (map (λ (nv-reg)
                                     (register-ref registers nv-reg))
                                   non-volatile-registers)])
       (let-values ([(result new-registers)
                     (parameterize ([runtime/stack-pointer stack-pointer]
                                    [runtime/flags         flags]
                                    [runtime/registers     registers]
                                    [runtime/memory        memory]
                                    [runtime/flag-ref      flag-ref]
                                    [runtime/registers     registers]
                                    [runtime/register-ref  register-ref]
                                    [runtime/memory-ref    memory-ref]
                                    [runtime/memory-set!   memory-set!]
                                    [runtime/register-set!
                                     (λ (register value)
                                       (runtime/registers
                                        (register-set (runtime/registers) register value)))]
                                    [runtime/register-set*!
                                     (λ pairs
                                       (runtime/registers
                                        (paired-fold 'runtime/register-set*! pairs (runtime/registers) register-set)))]
                                    [runtime/register-set/truncate!
                                     (λ (register value)
                                       (runtime/registers
                                        (register-set/truncate (runtime/registers) register value)))]
                                    [runtime/register-set*/truncate!
                                     (λ pairs
                                       (runtime/registers
                                        (paired-fold 'runtime/register-set*/truncate! pairs (runtime/registers) register-set/truncate)))]
                                    [current-input-port  (if uses-input?
                                                             (current-runtime-input-port)
                                                             (make-disabled-input-port name))]
                                    [current-output-port (if uses-output?
                                                             (current-runtime-output-port)
                                                             (make-disabled-output-port name))])
                       (values (apply procedure all-args)
                               (runtime/registers)))])
         (let ([new-nv-reg-values (map (λ (nv-reg)
                                         (register-ref new-registers nv-reg))
                                       non-volatile-registers)])
           (for ([nv-reg       (in-list non-volatile-registers)]
                 [old-nv-value (in-list preserved-values)]
                 [new-nv-value (in-list new-nv-reg-values)])
             (unless (equal? old-nv-value new-nv-value)
               (raise-user-error name
                                 "runtime function did not preserve non-volatile register: ~s" nv-reg)))
           (let ([randomized-volatile-registers
                   (for/fold ([registers new-registers])
                             ([v-reg (in-list volatile-registers)])
                     ;; NOTE: Racket's [random] function is limited to ranges no
                     ;; wider than 4294967087 = 2 ^ 32 - 209, for some reason.
                     ;; Since the point of this operation is to make it unlikely
                     ;; for the value to be something student code wouldn't have
                     ;; a problem with if used, we eliminate [0] as an option
                     ;; and opt for this range.
                     (register-set registers v-reg (random 1 4294967088)))])
             (values (convert-result result)
                     randomized-volatile-registers)))))]))

;; Whether the current runtime defines a function with the indicated name.
(define runtime-has-function?
  (case-lambda
    [(func-name)
     (runtime-has-function? (current-runtime) func-name)]
    [(runtime func-name)
     (hash-has-key? (runtime-external-functions runtime) func-name)]))

;; Returns a list of the functions currently defined in the runtime.
(define runtime-function-names
  (case-lambda
    [()
     (runtime-function-names (current-runtime))]
    [(runtime)
     (hash-keys (runtime-external-functions runtime))]))

;; Retrieves a [runtime-function?] from a [runtime?] by name.
(define runtime-function-ref
  (case-lambda
    [(func-name)
     (runtime-function-ref (current-runtime) func-name)]
    [(runtime func-name)
     (hash-ref (runtime-external-functions runtime) func-name #f)]))

;; A hash mapping runtime names to [runtime]s.
(define defined-runtimes (hash))

;; Modifies the [defined-runtimes] to include a new [runtime?].
(define (add-defined-runtime! runtime-name runtime)
  (set! defined-runtimes (hash-set defined-runtimes runtime-name runtime)))

;; Determines whether a [runtime?] with the given name exists.
(define (runtime-exists? runtime-name)
  (hash-has-key? defined-runtimes runtime-name))

;; Retrieves the [runtime?] with the given name if it exists. Otherwise, returns
;; [#f].
(define (name->runtime runtime-name)
  (hash-ref defined-runtimes runtime-name #f))

(begin-for-syntax
  ;; A list of runtime names that have already been defined.
  (define defined-runtime-names '()))

;; Defines a runtime, designed to emulate the capabilities of the C runtimes we
;; include with each course language.
;;
;;   (define-runtimes (id ...)
;;     [#:bits->value   id]
;;     [#:describe-bits id]
;;     [#:value->bits   id]
;;     [#:values    (value-def ...)]
;;     [#:types     (type-def  ...)]
;;     [#:functions (func-def  ...)]
;;     body ...)
;;
;;   value-def ::= (any/c integer?)
;;
;;   type-def  ::= (#:name id
;;                  [#:shift       any/c]
;;                  [#:tag         any/c]
;;                  [#:predicate   any/c]
;;                  [#:encode      any/c]
;;                  [#:decode      any/c]
;;                  [#:description any/c])
;;
;;   func-def  ::= ([#:uses-input]
;;                  [#:uses-output]
;;                  [#:argument-conversion-proc any/c]
;;                  [#:result-conversion-proc   any/c]
;;                  (id id ...)
;;                  body ...+)
;;
;; Each runtime is composed of:
;;
;;   1. Values, which map Racket values to bitwise representations.
;;   2. Types, which describe classes of values, their representations, their
;;      encoding and decoding schemes, and so on.
;;   3. Functions, which allow for calling Racket functions from the emulator.
;;   4. Additional definitions as needed, in the [body ...]. NOTE: These are
;;      executed when the runtime is initialized, not when it is used.
;;
;; All elements of a runtime are optional, i.e., it is valid to define a runtime
;; with no components.
(define-syntax (define-runtimes stx)

  (define-syntax-class value-spec
    (pattern [v b:exact-nonnegative-integer]))

  (define-syntax-class type-keyword
    (pattern (~or* #:name #:shift #:tag #:predicate #:encode #:decode #:description)))

  (define-syntax-class type-spec
    (pattern [(~alt (~seq _:type-keyword _:expr)) ...]))

  (define-syntax-class type-spec/shift+mask
    (pattern [(~alt (~once (~seq #:name name:id))
                    (~once (~seq #:shift shift-def))
                    (~seq _:type-keyword _:expr)) ...]
             #:with shift-name (format-id #'name "~a-shift" #'name)
             #:with mask-name  (format-id #'name "~a-mask"  #'name)
             #:with mask-def   #'(sub1 (arithmetic-shift 1 shift-name))))

  (define-syntax-class type-spec/tag+decode-pred
    (pattern (~and type:type-spec/shift+mask
                   [(~alt (~once (~seq #:tag tag-def))
                          (~seq _:type-keyword _:expr)) ...])
             #:with name             #'type.name
             #:with shift-name       #'type.shift-name
             #:with mask-name        #'type.mask-name
             #:with shift-def        #'type.shift-def
             #:with mask-def         #'type.mask-def
             #:with tag-name         (format-id #'name "~a-tag"  #'name)
             #:with decode-pred-name (format-id #'name "~a-tag?" #'name)
             #:with decode-pred      #'(λ (b) (= tag-name (bitwise-and b mask-name)))))

  (define-syntax-class type-spec/decode
    (pattern (~and type:type-spec/tag+decode-pred
                   [(~alt (~once (~seq #:decode decode-proc))
                          (~seq _:type-keyword _:expr)) ...])
             #:with name             #'type.name
             #:with shift-name       #'type.shift-name
             #:with mask-name        #'type.mask-name
             #:with tag-name         #'type.tag-name
             #:with shift-def        #'type.shift-def
             #:with mask-def         #'type.mask-def
             #:with tag-def          #'type.tag-def
             #:with decode-pred-name #'type.decode-pred-name
             #:with decode-pred      #'type.decode-pred))

  (define-syntax-class type-spec/tag+decode-pred-no-decode
    (pattern (~and type:type-spec/tag+decode-pred
                   [(~seq (~and _:type-keyword (~not #:decode)) _:expr) ...])
             #:with name             #'type.name
             #:with shift-name       #'type.shift-name
             #:with mask-name        #'type.mask-name
             #:with tag-name         #'type.tag-name
             #:with shift-def        #'type.shift-def
             #:with mask-def         #'type.mask-def
             #:with tag-def          #'type.tag-def
             #:with decode-pred-name #'type.decode-pred-name
             #:with decode-pred      #'type.decode-pred
             #:with err #`(error 'bits->value #,(format "cannot decode value of type ~s: ~~v" (syntax-e #'name)))))

  (define-syntax-class type-spec/description
    (pattern (~and type:type-spec/tag+decode-pred
                   [(~alt (~once (~seq #:description descr-proc))
                          (~seq _:type-keyword _:expr)) ...])
             #:with name             #'type.name
             #:with shift-name       #'type.shift-name
             #:with mask-name        #'type.mask-name
             #:with tag-name         #'type.tag-name
             #:with shift-def        #'type.shift-def
             #:with mask-def         #'type.mask-def
             #:with tag-def          #'type.tag-def
             #:with decode-pred-name #'type.decode-pred-name
             #:with decode-pred      #'type.decode-pred))

  (define-syntax-class type-spec/description-no-decode
    (pattern (~and type:type-spec/description
                   [(~seq (~and _:type-keyword (~not #:decode)) _:expr) ...])
             #:with name             #'type.name
             #:with shift-name       #'type.shift-name
             #:with mask-name        #'type.mask-name
             #:with tag-name         #'type.tag-name
             #:with shift-def        #'type.shift-def
             #:with mask-def         #'type.mask-def
             #:with tag-def          #'type.tag-def
             #:with decode-pred-name #'type.decode-pred-name
             #:with decode-pred      #'type.decode-pred
             #:with descr-proc       #'type.descr-proc))

  (define-syntax-class type-spec/encode-pred
    (pattern [(~alt (~once (~seq #:name name:id))
                    (~once (~seq #:predicate encode-pred))
                    (~seq _:type-keyword _:expr)) ...]))

  (define-syntax-class type-spec/encode-pred-no-encode
    (pattern (~and type:type-spec/encode-pred
                   [(~seq (~and _:type-keyword (~not #:encode)) _:expr) ...])
             #:with name        #'type.name
             #:with encode-pred #'type.encode-pred
             #:with err #`(error 'value->bits #,(format "cannot encode value of type ~s: ~~v" (syntax-e #'name)))))

  (define-syntax-class type-spec/encode
    (pattern (~and type:type-spec/encode-pred
                   [(~alt (~once (~seq #:encode encode-proc))
                          (~seq _:type-keyword _:expr)) ...])
             #:with name        #'type.name
             #:with encode-pred #'type.encode-pred))

  (define-syntax-class func-spec
    #:attributes (name [param 1] [body 1] uses-input? uses-output? convert-argument convert-result)
    (pattern (~and func-def
                   [(~alt (~optional (~and (~seq #:uses-input)  (~bind [uses-input?  #'#t]))
                                     #:defaults ([uses-input? #'#f]))
                          (~optional (~and (~seq #:uses-output) (~bind [uses-output? #'#t]))
                                     #:defaults ([uses-output? #'#f]))
                          (~optional (~seq #:argument-conversion-proc convert-argument)
                                     #:defaults ([convert-argument #'a86-value->signed-integer]))
                          (~optional (~seq #:result-conversion-proc convert-result)
                                     #:defaults ([convert-result #'(λ (x) x)]))
                          ) ...
                    (name:id param:id ...)
                    body ...])

             #:fail-when (and (zero? (length (attribute body)))
                              #'func-def)
             "expected non-empty runtime function body"))

  (syntax-parse stx
    [(_ (~and (runtime-name:id ...) runtime-names)
        (~alt (~optional (~seq #:bits->value bits->value-name:id)
                         #:defaults ([bits->value-name (format-id #'runtime-names "bits->value")]))
              (~optional (~seq #:describe-bits describe-bits-name:id)
                         #:defaults ([describe-bits-name (format-id #'runtime-names "describe-bits")]))
              (~optional (~seq #:value->bits value->bits-name:id)
                         #:defaults ([value->bits-name (format-id #'runtime-names "value->bits")]))
              (~optional (~seq #:values (value:value-spec ...))
                         #:defaults ([(value   1) '()]
                                     [(value.v 1) '()]
                                     [(value.b 1) '()]))
              (~optional (~seq #:types (type:type-spec ...))
                         #:defaults ([(type 1) '()]))
              (~optional (~seq #:functions (func:func-spec ...))
                         #:defaults ([(func                  1) '()]
                                     [(func.name             1) '()]
                                     [(func.uses-input?      1) '()]
                                     [(func.uses-output?     1) '()]
                                     [(func.convert-argument 1) '()]
                                     [(func.convert-result   1) '()]
                                     [(func.param            2) '()]
                                     [(func.body             2) '()]))
              ) ...
        body ...)

     (for ([name-stx (in-list (syntax->list #'(runtime-name ...)))])
       (when (memq (syntax-e name-stx) defined-runtime-names)
         (raise-syntax-error 'define-runtimes "runtime name already defined" name-stx)))

     (set! defined-runtime-names
           (append defined-runtime-names
                   (map syntax-e (syntax->list #'(runtime-name ...)))))

     (define/syntax-parse (type/shift+mask:type-spec/shift+mask ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/shift+mask #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/tag+decode-pred:type-spec/tag+decode-pred ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/tag+decode-pred #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/decode:type-spec/decode ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/decode #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/tag+decode-pred-no-decode:type-spec/tag+decode-pred-no-decode ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/tag+decode-pred-no-decode #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/description:type-spec/description ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/description #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/description-no-decode:type-spec/description-no-decode ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/description-no-decode #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/encode-pred:type-spec/encode-pred ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/encode-pred #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/encode-pred-no-encode:type-spec/encode-pred-no-encode ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/encode-pred-no-encode #'t] [_ #f]))
                    (syntax->list #'(type ...)))))
     (define/syntax-parse (type/encode:type-spec/encode ...)
       (filter values
               (map (λ (type-stx) (syntax-parse type-stx [t:type-spec/encode #'t] [_ #f]))
                    (syntax->list #'(type ...)))))

     #'(define-values (runtime-name ...)
         (let ()
           (define type/shift+mask.shift-name            type/shift+mask.shift-def) ...
           (define type/shift+mask.mask-name             type/shift+mask.mask-def) ...
           (define type/tag+decode-pred.tag-name         type/tag+decode-pred.tag-def) ...
           (define type/tag+decode-pred.decode-pred-name type/tag+decode-pred.decode-pred) ...
           (define (func.name func.param ...) func.body ...) ...

           (define (bits->value-name b)
             (cond [(= b (value->bits-name value.v)) value.v] ...
                   [(type/decode.decode-pred-name b) (type/decode.decode-proc b)] ...
                   [(type/tag+decode-pred-no-decode.decode-pred-name b)
                    ((~@ . type/tag+decode-pred-no-decode.err) b)] ...
                   [else (error 'bits->value-name "invalid bits: ~v" b)]))

           (define (describe-bits-name b)
             (cond [(type/description.decode-pred-name b) (type/description.descr-proc b)] ...
                   [else
                    (with-handlers ([exn? (λ _ (error 'describe-bits-name "cannot describe bits: ~v" b))])
                      (bits->value-name b))]))

           (define (value->bits-name v)
             (cond [(eq? v value.v) value.b] ...
                   [(type/encode.encode-pred v) (type/encode.encode-proc v)] ...
                   [(type/encode-pred-no-encode.encode-pred v)
                    ((~@ . type/encode-pred-no-encode.err) v)] ...
                   [else (error 'value->bits-name "cannot encode value: ~v" v)]))

           body ...

           (let* ([rt (runtime bits->value-name
                               describe-bits-name
                               value->bits-name
                               (hash (~@ 'func.name (runtime-function 'func.name
                                                                      (procedure-arity func.name)
                                                                      func.name
                                                                      func.convert-argument
                                                                      func.convert-result
                                                                      func.uses-input?
                                                                      func.uses-output?)) ...))]
                  [runtime-name rt] ...)
             (add-defined-runtime! 'runtime-name runtime-name) ...
             (values runtime-name ...))))]))
