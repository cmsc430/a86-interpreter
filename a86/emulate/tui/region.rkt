#lang racket

(provide ;; Regions and their fields.
         region-name
         region-coords
         region-size
         ;; Special syntax for use within region definitions.
         this-region
         this-region:coords
         this-region:size
         this-region:width
         this-region:height
         ;; Dynamic field/method access.
         call
         get
         ;; Dynamic element presence checking.
         has-element?
         has-field?
         has-method?
         ;; Define regions.
         define-region)

(require "term.rkt"

         racket/stxparam

         (for-syntax racket
                     racket/stxparam
                     racket/syntax
                     syntax/location
                     syntax/modcollapse
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/strip-context

                     (for-syntax racket/base)))

;; A region is no more than a name, a set of rectangle-defining coordinates, and
;; a bundle of fields and methods (see REGION DEFINITIONS below).
(struct region
  (name from-x from-y to-x to-y elements-hash))

;; Returns the coordinates of the given region as a list of four elements: the
;; top-left x and y coordinates followed by the bottom-right x and y
;; coordinates.
(define (region-coords r)
  (match r
    [(region _ from-x from-y to-x to-y _)
     (list from-x from-y to-x to-y)]))

;; Returns the size of the region as a list of two elements: the width, and the
;; height. Sizes are given in terminal-pixels.
(define (region-size r)
  (match r
    [(region _ from-x from-y to-x to-y _)
     (list (add1 (- to-x from-x))
           (add1 (- to-y from-y)))]))

;; Region fields just hold values. We use the indirection of the struct so we
;; can tell if a field has been initialized or not.
;;
;; Fields are not mutable from outside the region, but methods of the region can
;; update the values of fields.
(struct region-field (value))
;; Region methods are just functions with implicit access to the region through
;; syntax parameters.
(struct region-method (function))

;; Dynamically looks up the value of a field within a region. Raises an error if
;; the argument is not a region, if the argument is a region but the region is
;; uninitialized, if the region does not have a field with the indicated name,
;; or if the region does have a field with this name but the field is
;; uninitialized.
(define-syntax (get stx)
  (syntax-parse stx
    [(_ r field-id)
     #'(begin
         (unless r
           (error 'get "region not initialized: ~a" 'r))
         (unless (region? r)
           (error 'get "not a region: ~v" r))
         (match (hash-ref (region-elements-hash r) field-id 'no-field)
           [(region-field v) v]
           ['no-field
            (error 'get
                   "field not defined for region ~a: ~a"
                   'r
                   field-id)]
           [#f
            (error 'get
                   "field not initialized for region ~a: ~a"
                   'r
                   field-id)]))]))

;; Dynamically calls a method within a region. Raises an error if the first
;; argument is not a region, if the argument is a region but the region is
;; uninitialized, or the region does not have a method with the given name.
;;
;; FIXME: I'm not sure the dynamic call will actually work, because the special
;; syntax will not be parameterized within the anonymous function being
;; invoked. I think the anonymous functions need to be created with the
;; parameterization, rather than the prefixed method functions.
(define-syntax (call stx)
  (syntax-parse stx
    [(_ r method-id args ...)
     #'(begin
         (unless r
           (error 'call "region not initialized: ~a" 'r))
         (match (hash-ref (region-elements-hash r) method-id 'no-method)
           [(region-method f) (f args ...)]
           ['no-method
            (error 'call
                   "method not defined for region ~a: ~a"
                   'r
                   method-id)]))]))

;; Answers whether the given region has an element with the indicated name,
;; which should be given as a symbol.
(define-syntax (has-element? stx)
  (syntax-parse stx
    [(_ r method-id)
     #'(begin (unless r
                (error 'has-element? "region not initialized: ~a" 'r))
              (hash-ref (region-elements-hash r) method-id #f))]))

;; Answers whether the given region has a field with the indicated name,
;; which should be given as a symbol. Returns [#f] if the element exists but is
;; not a field.
(define-syntax (has-field? stx)
  (syntax-parse stx
    [(_ r field-id)
     #'(begin (unless r
                (error 'has-field? "region not initialized: ~a" 'r))
              (let ([f (has-element? r field-id)])
                (and f
                     (region-field? f))))]))

;; Answers whether the given region has a method with the indicated name,
;; which should be given as a symbol. Returns [#f] if the element exists but is
;; not a method.
(define-syntax (has-method? stx)
  (syntax-parse stx
    [(_ r field-id)
     #'(begin (unless r
                (error 'has-method? "region not initialized: ~a" 'r))
              (let ([m (has-element? r method-id)])
                (and m
                     (region-method? m))))]))

;; Helps define special syntax for use within a region definition.
(define-syntax (define-region-keywords stx)
  (syntax-parse stx
    [(_ id:id ...+)
     #'(begin
         (define-syntax-parameter id
           (λ (stx)
             (syntax-case stx ()
               [val (identifier? #'val)
                    (raise-syntax-error #f "use of region keyword outside region context" stx)]))) ...)]))

;; We define some special syntax for use within a region.
(define-region-keywords
  this-region
  this-region:coords this-region:size
  this-region:width this-region:height)

(module* utility #f
  (provide (struct-out region-field)
           (struct-out region-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REGION DEFINITION
;;
;; "Regions" are rectangular sections of the terminal that have their own
;; distinct behavior. Regions are similar to "windows" in emacs parlance, if we
;; take the entire text-based user interface to be a "frame" in the same
;; vocabulary. This syntax macro enables easy definition of regions with
;; encapsulated data and behavior.
;;
;; Region definitions work a lot like class definitions. Regions can have fields
;; and methods (collectively called the "elements"), which have implicit access
;; to the region itself as well as all the other elements. This allows for
;; easily writing functions with shared data, which can be immensely beneficial
;; in implementing a user interface.
;;
;; NOTE: Although there are similarities to classes, there are many
;; distinctions. One important distinction is that all fields and methods of a
;; region are "public" in the sense that they will be accessible outside the
;; region.
;;
;; TODO: Is it worth adding a notion of "private" fields and methods? It could
;; be done like Python's, i.e., by using a special prefix or suffix in the
;; identifiers themselves. For now I think this is not important, but maybe we
;; can revisit it later.
;;
;; The syntax of region definitions is:
;;
;;     (define-region
;;       region-id
;;       #:width-spec width
;;       #:height-spec height
;;       [(define-pre-init (param ...) body ...+)]
;;       [(define-post-init (param ...) body ...+)]
;;       (define-field field-id [body]) ...
;;       (define-method (method-id param ...) body ...+) ...)
;;
;;     region-id, param, field-id, method-id :: identifier
;;     width, height                         :: size-spec*
;;
;;     *The size-spec is explained further below.
;;
;; (Note that, except for the [region-id], the forms may be given in any order.)
;;
;; Region methods do not support advanced parameter binding. In other words,
;; only simple identifiers are allowed as [param]s. Within the [body]s, methods
;; can be called like regular functions using their defined names, e.g.,
;; [(method-id arg ...)].
;;
;; Fields can be defined uninitialized by simply not specifying a value. Such
;; fields must be initialized by the end of the initialization sequence. This
;; can be done by using the [define-pre-init] or [define-post-init] forms.
;;
;; The [body]s of the pre-initializer function (which is optional) will be
;; evaluated before any [region?] is initialized at all, meaning that you
;; probably don't want to refer to [this-region] within those [body]s. But maybe
;; you do, I don't know.
;;
;; The [body]s of the post-initializer function (which is optional) will be
;; evaluated after the [region?] has been initialized and the (initialized)
;; fields and methods have been set. It is guaranteed that [this-region] will be
;; properly set by this point.
;;
;; Within the [body]s, fields can be modified via regular [set!] syntax. For
;; example, one might perform [(set! answer 42)] to set the field named [answer]
;; within the current region to the value [42].
;;
;; After evaluation of the [define-region] form, a number of identifiers will
;; become available in the surrounding context:
;;
;;     region-id:initialize!
;;     region-id:field-id ...
;;     region-id:methood-id ...
;;
;;
;; SPECIAL SYNTAX
;;
;; Within any [body], the syntax [this-region] will refer to the region's value,
;; which is a [region?]. Additionally, [this-region:coords] will be bound to a
;; list consisting of the coordinates of [this-region] in the same form as those
;; produced by [region-coords]. Lastly, [this-region:size] will be bound to a
;; pair of the width and height of the region.
;;
;; The [body]s of fields and methods can refer to any of the [field-id]s or
;; [method-id]s directly. The [field-id]s must be used as identifiers, while the
;; [method-id]s must be called as functions with the appropriate quantity of
;; arguments. The fields can be updated by using regular [set!] syntax, e.g.,
;; [(set! field-id new-value)] will replace the current value of [field-id] with
;; [new-value]. NOTE: The [new-value] expression will be evaluated prior to
;; setting the value in place, allowing you to reference a field recursively
;; without an infinite loop.
;;
;;
;; SIZE SPECIFICATIONS (SIZE-SPEC)
;;
;; Regions support a somewhat complex grammar for defining what size they ought
;; to try to be. These size specifications are used during the search for an
;; empty space on the screen during initialization.
;;
;; The grammar for size specifications is as follows:
;;
;;     size      ::= percentage?
;;                |  exact-positive-integer?
;;                |  (range size-expr size-expr)
;;                |  size-expr
;;
;;     size-expr ::= (percentage? size-expr)
;;                |  (math-op size-expr)
;;                |  size-atom
;;
;;     size-atom ::= exact-positive-integer?
;;                |  (sizeof region-id)
;;                |  avail | available
;;                |  full | total | max
;;
;;     math-op   ::= + | - | * | /
;;
;; Some size specification examples, explained:
;;
;;     37           Exactly 37 terminal-pixels.
;;
;;     25%          Converted to [(25% total)].
;;
;;     avail        Bound during each iteration of the space search to the
;;     available    number of terminal-pixels available along this axis.
;;
;;     full         Bound during the space search to the size of the terminal
;;     total        along this axis. This number is fixed for each search (i.e.,
;;     max          it is not re-evaluated during each iteration).
;;
;;     (p% s)       Converted to [(* (p / 100) s)].
;;
;;     (range a b)  Finds a space at least [a] terminal-pixels in size, but up
;;                  to [b] terminal-pixels.
;;
;;                  NOTE: This range is inclusive, unlike Racket's [range].
;;
;;     (sizeof r)   Evaluates to the size of the region [r] along the same axis.
;;
;;                  NOTE: If the region [r] is not initialized, this will result
;;                  in an error.
;;
;;
;; INITIALIZATION
;;
;; The [region-id:initialize!] function triggers the initialization sequence,
;; the steps of which are detailed below. The function is invoked with _n_ + _m_
;; arguments, where _n_ is the number of parameters defined in [define-pre-init]
;; and _m_ is the number of parameters defined in [define-post-init]. The
;; pre-init and post-init parameter names are prefixed internally to avoid
;; collisions. The first _n_ arguments to [region-id:initialize!] will be passed
;; to the pre-init [body]s, while the last _m_ arguments will be passed to the
;; post-init [body]s.
;;
;; The initialization sequence works like this:
;;
;;   0. The internal region identifier is bound to [#f], meaning [this-region]
;;      will produce [#f] for now.
;;   1. The pre-init [body]s are evaluated. Note that references to
;;      uninitialized fields at this point will produce errors.
;;   2. The main initialization sequence takes place, which consists of a number
;;      of sub-steps.
;;
;;        a. The current screen size is obtained from [term:screen-size].
;;        b. A number of columns and rows is determined by converting the given
;;           size specifications ([width] and [height]) within the context of
;;           the now-determined screen size.
;;        c. The screen is searched for an empty rectangle meeting the derived
;;           specifications. If no such space can be found, an error is raised.
;;        d. The internal [region?] is initialized with these derived
;;           specifications, and is installed into the region state via
;;           [regions:install].
;;
;;   3. The post-init [body]s are evaluated.
;;   4. Fields defined without an initial value are checked. If any are found to
;;      still be without a value, an error is raised.
;;
;; Within any of the initialization [body] forms, all of the field and method
;; names will be bound. Note that accessing the value of an uninitialized field
;; results in an error. The region itself ([this-region]) will be [#f] until
;; after the main initialization step (2d).
;;
;;
;; EXTERNAL IDENTIFIERS
;;
;; After defining a region with [define-region] and initializing it with the
;; [region-id:initialize!] function, the elements become accessible via prefixed
;; forms of their internal names. For example, a method named [fly-hovercar]
;; within a region named [my-region] will be available externally as
;; [my-region:fly-hovercar]. Fields are still accessed as bare identifiers, and
;; methods are called as normal functions which implicitly have access to
;; [this-region].
;;
;; Sometimes, it may be desirable to dynamically refer to elements of a region.
;; For example, if you have a set of regions that you have implemented with a
;; similar interface (e.g., they all have a method [count-eels]), you may want
;; to call all of the regions' same-named methods. This is not possible using
;; the prefixed identifier forms, but it _is_ possible via [get] and [call]. For
;; this example, you might do:
;;
;;     (for ([r list-of-regions])
;;       (call r 'count-eels))
;;
;; In fact, the prefixed identifier forms are syntactic sugar for this process.
(define-syntax (define-region stx)

  (define (extract-module-dependencies stx)
    (set->list
     (let recurse ([e stx])
       (match e
         [(? identifier? (app identifier-binding (list _ _ nominal-from-mod _ _ _ _)))
          (set (collapse-module-path-index nominal-from-mod))]
         [(? syntax?)
          (recurse (syntax-e e))]
         [(cons a b)
          (set-union (recurse a) (recurse b))]
         [_ (set)]))))

  (define (make-prefixed-id prefix-id-stx suffix-id-stx #:source [source #f])
    (format-id prefix-id-stx #:source (or source suffix-id-stx)
               "~a:~a" (syntax-e prefix-id-stx) (syntax-e suffix-id-stx)))

  (define ((map-prefixed-id prefix-id-stx) suffix-id-stx)
    (make-prefixed-id prefix-id-stx suffix-id-stx #:source suffix-id-stx))

  (syntax-parse stx
    [(_ pre-forms ...
        ((~datum define-fields) (field-id ...) (~optional (init-val ...)))
        post-forms ...)
     (if (attribute init-val)
         (begin (unless (= (length (syntax->list #'(init-val ...)))
                           (length (syntax->list #'(field-id ...))))
                  (raise-syntax-error #f "field requires initial value"
                                      (list-ref (syntax->list #'(field-id ...))
                                                (length (syntax->list #'(init-val ...))))))
                #'(define-region
                    pre-forms ...
                    (define-field field-id init-val) ...
                    post-forms ...))
         #'(define-region
             pre-forms ...
             (define-field field-id) ...
             post-forms ...))]
    [(_ id:id
        (~alt (~once (~seq #:width-spec width-spec))
              (~once (~seq #:height-spec height-spec))
              (~optional ((~datum define-pre-init) (pre-init-arg:id ...) pre-init-body ...+)
                         #:defaults ([(pre-init-arg  1) '()]
                                     [(pre-init-body 1) `(,#'(void))]))
              (~optional ((~datum define-post-init) (post-init-arg:id ...) post-init-body ...+)
                         #:defaults ([(post-init-arg  1) '()]
                                     [(post-init-body 1) `(,#'(void))]))
              ;; TODO: Add forms for defining multiple fields/methods at once.
              ((~datum define-field) no-init-field-id:id)
              ((~datum define-field) field-id:id field-init-val)
              #;((~datum define-method) (method-id:id method-param:id ...) method-body ...+)
              ((~datum define-method) (method-id:id method-formal:formal ...) method-body ...+))
        ...)
     #:with prefixed-module-id                  (make-prefixed-id #'id #'module)
     #:with prefixed-region-id                  (make-prefixed-id #'id #'region)
     #:with prefixed-coords                     (make-prefixed-id #'id #'coords)
     #:with prefixed-size                       (make-prefixed-id #'id #'size)
     #:with prefixed-initialize!                (make-prefixed-id #'id #'initialize!)
     #:with prefixed-uninitialized-element-hash (make-prefixed-id #'id #'uninitialized-element-hash)

     #:with (externally-required-module ...)
     (extract-module-dependencies #'(#f  ;; Just to fix the indentation. :')
                                     pre-init-body ...
                                     post-init-body ...
                                     field-init-val ...
                                     method-body ... ...))

     #:with require-form (replace-context stx #'(require 'prefixed-module-id))
     #:with provide-form (replace-context stx #'(provide (all-from-out 'prefixed-module-id)))
     #:with (inner-requires ...)
     (let*-values ([(this-module-path) (quote-module-path)]
                   [(this-module-dir _ __) (split-path this-module-path)])
       (list #`(file #,(path->string this-module-path))
             #`#,(quote-module-path utility)
             #`(file #,(path->string (build-path this-module-dir "term.rkt")))
             #`(file #,(path->string (build-path this-module-dir "region-state.rkt")))
             #'racket/match))

     #:with (all-field-id ...) (append (syntax->list #'(no-init-field-id ...))
                                       (syntax->list #'(field-id ...)))
     #:with (get-field-id ...) (map    (map-prefixed-id #'get)
                                       (syntax->list #'(all-field-id ...)))
     #:with (element-id ...)   (append (syntax->list #'(all-field-id ...))
                                       (syntax->list #'(method-id ...)))

     #:with ((method-param ...) ...)
     (map (λ (fs-stx)
            (flatten (map (λ (f-stx) (syntax->list f-stx))
                          (syntax->list fs-stx))))
          (syntax->list #'((method-formal ...) ...)))

     #:with (prefixed-element-id ...)    (map (map-prefixed-id #'id)   (syntax->list #'(element-id ...)))
     #:with (prefixed-pre-init-arg ...)  (map (map-prefixed-id #'pre)  (syntax->list #'(pre-init-arg ...)))
     #:with (prefixed-post-init-arg ...) (map (map-prefixed-id #'post) (syntax->list #'(post-init-arg ...)))
     #:with (prefixed-all-field-id ...)  (map (map-prefixed-id #'id)   (syntax->list #'(all-field-id ...)))
     #:with (prefixed-method-id ...)     (map (map-prefixed-id #'id)   (syntax->list #'(method-id ...)))

     #:with (special-syntaxes-parameterized ...)
     #'([this-region        (make-rename-transformer #'prefixed-region-id)]
        [this-region:coords (make-rename-transformer #'prefixed-coords)]
        [this-region:size   (make-rename-transformer #'prefixed-size)]
        [this-region:width
         (λ (stx)
           (syntax-case stx ()
             [val (identifier? #'val)
                  #'(first this-region:size)]))]
        [this-region:height
         (λ (stx)
           (syntax-case stx ()
             [val (identifier? #'val)
                  #'(second this-region:size)]))])

     #:with (uninitialized-parameterized-elements ...)
     ;; TODO: Should [this-region] &c be bound in the uninitialized phase?
     #'(special-syntaxes-parameterized ...
        [all-field-id
         (make-set!-transformer
          (λ (stx)
            (syntax-case stx (set!)
              [(set! id v)
               #'(hash-set! prefixed-uninitialized-element-hash
                            'id (region-field v))]
              [id
               (identifier? #'id)
               #'(match (hash-ref prefixed-uninitialized-element-hash 'id)
                   [(region-field v) v])])))] ...
        [method-id
         (λ (stx)
           (syntax-case stx ()
             [(id . args)
              #`(match (hash-ref prefixed-uninitialized-element-hash 'id)
                  [(region-method f)
                   (f #,@#'args)])]))] ...)

     #:with (parameterized-elements ...)
     #'(special-syntaxes-parameterized ...
        [all-field-id
         (make-set!-transformer
          (λ (stx)
            (syntax-case stx (set!)
              [(set! id v)
               #'(hash-set! (region-elements-hash this-region)
                            'id (region-field v))]
              [id
               (identifier? #'id)
               ;; TODO: Should this be implemented as primitive functionality
               ;; instead of relying on [get]?
               #'(get this-region 'id)])))] ...
        [method-id
         (λ (stx)
           (syntax-case stx ()
             [(id . args)
              ;; TODO: Should this be implemented as primitive functionality
              ;; instead of relying on [call]?
              #`(call this-region 'id #,@#'args)]))] ...)

     ;; We replace the context so identifiers are properly bound. It is unclear
     ;; whether there is a better alternative, but trust that the code won't
     ;; work without this.
     (replace-context
      stx
      #'(begin
          ;; Define an internal module. This lets us write normal code but then
          ;; only expose the specific things desired.
          (module prefixed-module-id racket/base
            (provide prefixed-region-id ;; TODO: Remove.
                     prefixed-initialize!
                     prefixed-element-id ...)

            (require racket/stxparam
                     externally-required-module ...
                     inner-requires ...
                     (for-syntax racket/base))

            ;; The syntax parameters are allow the elements' identifiers to be
            ;; used in the implementation of the elements themselves.
            (define-syntax-parameter element-id #'#f) ...

            ;; The region itself. This will not be exported from the module.
            (define prefixed-region-id #f)

            ;; The initial values are set. This hash is used during the
            ;; pre-initialization phase, and is then inserted into the region
            ;; object in the main stage of initialization.
            (define prefixed-uninitialized-element-hash (make-hash))
            (syntax-parameterize (uninitialized-parameterized-elements ...)
              (hash-set! prefixed-uninitialized-element-hash
                         'element-id
                         #f) ...
              (hash-set! prefixed-uninitialized-element-hash
                         'field-id
                         (region-field field-init-val)) ...
              (hash-set! prefixed-uninitialized-element-hash
                         'method-id
                         (region-method (λ (method-param ...)
                                          method-body ...))) ...
              ;; NOTE: This prevents syntax errors due to a missing body if the
              ;; region does not define any elements.
              (void))

            ;; The user may define pre/post-init code. Parameters added to these
            ;; functions must be passed into the [initialize!] function.
            (define (pre-init  pre-init-arg  ...)
              (syntax-parameterize (uninitialized-parameterized-elements ...)
                pre-init-body  ...))
            (define (post-init post-init-arg ...)
              ;; NOTE: We use the regular [parameterized-elements] here because
              ;; the region should now exist.
              (syntax-parameterize (parameterized-elements ...)
                post-init-body ...))

            ;; The initializer is the only way to put a value in
            ;; [prefixed-region-id]. It requires that the terminal be
            ;; instantiated.
            (define (prefixed-initialize! prefixed-pre-init-arg ...
                                          prefixed-post-init-arg ...)
              ;; Make element references work.
              (syntax-parameterize (uninitialized-parameterized-elements ...)
                ;; Call the pre-initialization code.
                (pre-init prefixed-pre-init-arg ...)
                ;; Actually initialize the region.
                (let-values ([(screen-cols screen-rows) (term:screen-size)]
                             [(test-width test-height)
                              (values
                               (λ (cur-avail-x)
                                 (parameterize ([available-x cur-avail-x])
                                   ((parse-size-spec #:width width-spec))))
                               (λ (cur-avail-y)
                                 (parameterize ([available-y cur-avail-y])
                                   ((parse-size-spec #:height height-spec)))))])
                  (parameterize ([total-x screen-cols]
                                 [total-y screen-rows])
                    (match (regions:find-empty-rectangle test-width test-height)
                      [#f (error 'prefixed-initialize! "unable to find empty space for region ~v" 'id)]
                      [(list from-x from-y to-x to-y)
                       (term:set-current-pos! 150 1)
                       (term:display
                        "found space for region! (~v, ~v) to (~v, ~v)"
                        from-x from-y to-x to-y)
                       (let ([r (region 'id from-x from-y to-x to-y
                                        prefixed-uninitialized-element-hash)])
                         (set! prefixed-region-id r)
                         (regions:install r))])))
                ;; Call the post-initialization code.
                (post-init prefixed-post-init-arg ...))
              ;; NOTE: All fields must be initialized.
              (unless (region-field? (hash-ref (region-elements-hash prefixed-region-id)
                                               'no-init-field-id))
                (error 'prefixed-initialize! "field not initialized: ~a" 'no-init-field-id)) ...)

            (define-syntax prefixed-coords
              (λ (stx)
                (syntax-case stx ()
                  [val
                   (identifier? #'val)
                   #'(syntax-parameterize (parameterized-elements ...)
                       (region-coords this-region))])))

            (define-syntax prefixed-size
              (λ (stx)
                (syntax-case stx ()
                  [val
                   (identifier? #'val)
                   #'(syntax-parameterize (parameterized-elements ...)
                       (region-size this-region))])))

            ;; Field IDs are actually special syntax so they can be used as
            ;; identifiers rather than functions. These prefixed identifiers are
            ;; just wrappers for the getter functions defined below.
            (define-syntax prefixed-all-field-id
              (λ (stx)
                (syntax-case stx ()
                  [val
                   (identifier? #'val)
                   #'(syntax-parameterize (parameterized-elements ...)
                       (get-field-id))]))) ...

            ;; TODO: Should this be implemented as primitive functionality
            ;; instead of relying on [get]?
            (define (get-field-id) (get prefixed-region-id 'all-field-id)) ...

            ;; The exported method names are prefixed, and simply wrap calls to
            ;; the [call] functionality.
            (define (prefixed-method-id method-param ...)
              (syntax-parameterize (parameterized-elements ...)
                ;; TODO: Should this be implemented as primitive functionality
                ;; instead of relying on [call]?
                (call prefixed-region-id 'method-id method-param ...))) ...
            ) ;; End of module definition.
          ;; We require and provide from the module automatically.
          require-form
          provide-form))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SIZE SPECIFICATION
;;
;; A size specification is syntax that is transformed into a function that can
;; be evaluated during region initialization in a context where the total and
;; available sizes in the current dimension are parameterized. The result of
;; evaluating the function is either a number representing the size the region
;; should be given, or else [#f] indicating that the region does not accept the
;; indicated size (because it is too small).
;;
;; Size specifications can make use of the special syntax [available] and
;; [total]. The [available] identifier is parameterized with the current amount
;; of empty terminal-pixels along the axis of the current dimension. The [total]
;; identifier is parameterized with the size of the screen in the current
;; dimension.
(define-syntax available
  (λ (stx)
    (syntax-case stx ()
      [id (identifier? #'id) #'(current-get-available)])))
(define-syntax total
  (λ (stx)
    (syntax-case stx ()
      [id (identifier? #'id) #'(current-get-total)])))
;; These require the following syntax parameters to be set.
(define-syntax-parameter current-get-available #'#f)
(define-syntax-parameter current-get-total #'#f)
;; For the special syntax to work, four parameters must be set dynamically:
;;
;;     available-x    Number of available terminal-pixels in the x direction.
;;     available-y    Number of available terminal-pixels in the y direction.
;;     total-x        Size of the screen in the x direction.
;;     total-y        Size of the screen in the y direction.
(define available-x (make-parameter #f))
(define available-y (make-parameter #f))
(define total-x     (make-parameter #f))
(define total-y     (make-parameter #f))
;; The size-specs are transformed roughly according to the following patterns:
;;
;;   37                     => (and (>= available 37)
;;                                  37)
;;   25%                    => (and (>= available (* (/ 25 100) total))
;;                                  (* (/ 25 100) total))
;;   avail                  => available
;;   full                   => (and (>= available total)
;;                                  total)
;;   (25% avail)            => (* (/ 25 100) available)
;;   (37% 42)               => (and (>= available 16)
;;                                  16)
;;   (range 3 9)            => (and (>= available 3)
;;                                  (min available 9))
;;   (range 3 avail)        => (and (>= available 3)
;;                                  available)
;;   (range avail 7)        => n/a
;;   (range (23% avail)
;;          (74% total))    => (and (>= available (* (/ 23 100) available))
;;                                  (min available (* (/ 74 100) total)))

(begin-for-syntax
  (define (percentage? e)
    (and (symbol? e)
         (match (symbol->string e)
           [(regexp #px"^(\\d+)%$" (list _ ns))
            (let ([n (string->number ns)])
              (and (<= 1 n 100)
                   n))]
           [_ #f])))

  (define-syntax-class percentage
    (pattern p:id
             #:fail-unless (percentage? (syntax-e #'p)) "not a percentage"
             #:attr value (/ (percentage? (syntax-e #'p)) 100))))

(define-syntax (parse-size-spec stx)
  (syntax-parse stx
    [(_ #:width spec)
     #'(syntax-parameterize ([current-get-available (make-rename-transformer #'available-x)]
                             [current-get-total     (make-rename-transformer #'total-x)])
         (parse-size spec))]
    [(_ #:height spec)
     #'(syntax-parameterize ([current-get-available (make-rename-transformer #'available-y)]
                             [current-get-total     (make-rename-transformer #'total-y)])
         (parse-size spec))]))

(define-syntax (parse-size stx)
  (syntax-parse stx
    #:datum-literals (full total max range)
    #;[(_ p:percentage)         #`(λ () (let ([v (round (* #,(attribute p.value)
                                                         total))])
                                        (and (>= available v)
                                             v)))]
    ;; [(_ n:nat)                #'(λ () (and (>= available n) n))]
    ;; [(_ (~or full total max)) #'(λ () (and (>= available total) total))]
    [(_ p:percentage)  #'(parse-size (p total))]
    [(_ (range e1 e2)) #'(λ () (let ([lo (parse-size-expr e1)]
                                     [hi (parse-size-expr e2)])
                                 (unless (<= lo hi)
                                   (error 'size-spec "range not increasing: (~v, ~v)" lo hi))
                                 (and (>= available lo)
                                      (min available hi))))]
    [(_ e)             #'(λ () (let ([n (parse-size-expr e)])
                                 (and (>= available n)
                                      n)))]))

(define-syntax (parse-size-expr stx)
  (syntax-parse stx
    #:datum-literals (+ - * /)
    [(_ (p:percentage e))            #`(let* ([s (round (* (parse-size-expr e)
                                                           #,(attribute p.value)))])
                                         (and (>= available s) s))]
    [(_ ((~and (~or + - * /) op) e)) #'(op (parse-size-expr e))]
    [(_ e)                           #'(parse-size-atom e)]))

(define-syntax (parse-size-atom stx)
  (syntax-parse stx
    #:datum-literals (avail available full total max sizeof)
    [(_ n:nat)                 #'n]
    [(_ (sizeof region-id:id)) (raise-syntax-error #f "not currently supported" stx)]
    [(_ (~or avail available)) #'available]
    [(_ (~or full total max))  #'total]
    [_                         (raise-syntax-error #f "could not parse size spec" stx)]))



















;; Creates a [region?], an installer for the region, and sets up all the
;; "methods" within the region that can be used externally.
;;
;; Consider a region definition:
;;
;;   (define-region my-region
;;     #:width-spec 20%
;;     #:height-spec available
;;     (define-field refresh-count 0)
;;     (define-method (refresh from-x from-y to-x to-y)
;;       (set! refresh-count (add1 refresh-count))
;;       (draw-some-stuff from-x to-x)))
;;
;; Within the methods of the region, the fields and other methods are visible
;; with their bare names, but these names are obfuscated externally by prefixing
;; them with the region's name. So, for example, the [refresh] method would be
;; invoked externally via a call like [(my-region:refresh 1 1 4 5)].
;;
;; Within the context of any of the fields or methods, the special syntax
;; [this-region] is available, and it refers to the [region?] corresponding to
;; the current region.
;;
;; Methods are also available dynamically through a [send-region] mechanism. The
;; region contains a [hash?] mapping method and field names to their functions
;; such that [(send-region my-region 'method-id arg1 arg2)] corresponds to
;; [(my-region:method-name arg1 arg2)]. This is useful for dispatching the same
;; arguments to similar methods defined in multiple regions (e.g., invoking a
;; refreshing method for all regions to update the screen).
;;
;; TODO: I think this can be encapsulated in a module that is then automatically
;; [require]d from. That might be easier than trying to wrap it all in a
;; function or something.
#;(define-syntax (define-region stx)
    (define-syntax-class size-spec
      (pattern (~datum available))
      (pattern e)
      ;; TODO
      #;(pattern ))

    (syntax-parse stx
      [(_ region-id:id
          (~alt (~once (~seq #:width-spec  width-spec:size-spec))
                (~once (~seq #:height-spec height-spec:size-spec))
                ((~datum define-field) no-init-field-id:id)
                ((~datum define-field) field-id:id field-init-val)
                ((~datum define-method) (method-id:id param:id ...) method-body ...+)) ...)
       (define (make-prefix-id suffix-id-stx)
         (format-id suffix-id-stx #:source suffix-id-stx "~a:~a" (syntax-e #'region-id) (syntax-e suffix-id-stx)))
       (with-syntax
         ([region-id
           (format-id #'region-id #:source #'region-id "~a:region" (syntax-e #'region-id))]
          [module-id
           (format-id #'region-id #:source #'region-id "~a:module" (syntax-e #'region-id))]
          [(prefix:no-init-field-id ...)
           (map make-prefix-id (syntax->list #'(no-init-field-id ...)))]
          [(prefix:field-id ...)
           (map make-prefix-id (syntax->list #'(field-id ...)))]
          [(prefix:method-id ...)
           (map make-prefix-id (syntax->list #'(method-id ...)))])

         ;; 1. Rewrite [region?] struct definition with dispatch capabilities.
         ;;
         ;;     I think we can model this through a simple symbol-based hash
         ;;     lookup functionality. The behavior for fields will need to be
         ;;     considered, but for methods it could be something like:
         ;;
         ;;         (call the-region 'method-id args ...)
         ;;
         ;;     Which translates into a lookup of ['method-id] within the method
         ;;     hash, acquiring an anonymous function (?), and then calling that
         ;;     function with the args. Within the scope of the method body, some
         ;;     syntactic forms may be [syntax-parameterize]d, such as
         ;;     [this-region] so they don't have to be passed in as arguments.
         ;;
         ;;     Field lookup is similar but perhaps a bit different. I guess we
         ;;     could do:
         ;;
         ;;         (access the-region 'field-id)
         ;;
         ;;     This would look up the value in the (mutable) hash.
         ;;
         ;; 2. Provide internal syntax for referring to the region's elements.
         ;;
         ;;     Within method bodies (and fields? unsure), the other elements of
         ;;     the region should be accessible without being written as a symbol,
         ;;     and without any special prefix. For example:
         ;;
         ;;         (define-method (add-coords)
         ;;           (sum (region-coords this-region)))
         ;;         (define-method (do-thing)
         ;;           (add1 (add-coords)))
         ;;
         ;;     Note that the [do-thing] method refers to the [add-coords] method
         ;;     by name. Similarly, the [add-coords] method is able to refer to
         ;;     [this-region].
         ;;
         ;; 3. Provide external bindings for the region's elements.
         ;;
         ;;     Within any scope where the region is defined, the fields and
         ;;     methods should still be accessible. Instead of requiring the
         ;;     lengthy syntax from (1), we can instead provide a prefixed binding
         ;;     for each of the elements:
         ;;
         ;;         (call the-region 'method-id args ...)
         ;;
         ;;     becomes
         ;;
         ;;         (the-region:method-name args ...)
         ;;
         ;;
         ;;
         ;; IMPLEMENTATION
         ;;
         ;; The implementation of this system requires a few things. First, the
         ;; region struct needs to be redefined to have a hash for mapping the
         ;; element names to their values. The new [region] struct will have:
         ;;
         ;;     name
         ;;     from-x from-y to-x to-y
         ;;     num-cols num-rows
         ;;     elements-hash
         ;;
         ;; NOTE: The keymap has been removed. I think this actually should only
         ;; exist at the top level, and the top level will be responsible for
         ;; calling the various methods as needed. This is less modular, but this
         ;; isn't meant to be a perfectly modular system.
         ;;
         ;; After revising the region struct, the [define-region] macro needs to
         ;; be implemented.

         #`(begin
             (define the-region #f)
             (define no-init-field-id (thunk (error 'no-init-field-id "uninitialized"))) ...
             ;; FIXME: doesn't work for some reason
             (define field-id 'field-init-val) ...
             (define (method-id param ...) method-body ...) ...



             #;(module module-id racket
                 #;(require (for-syntax racket))
                 #;(provide (rename-out [the-region region-id]
                                        [no-init-field-id prefix:no-init-field-id] ...
                                        [field-id prefix:field-id] ...
                                        [method-id prefix:method-id] ...))
                 (define the-region #f)
                 (define no-init-field-id (error 'no-init-field-id "uninitialized")) ...
                 ;; FIXME: doesn't work for some reason
                 (define field-id 'field-init-val) ...
                 (define (method-id param ...) method-body ...) ...)))]))
