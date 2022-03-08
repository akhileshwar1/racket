#lang racket

;; =================================================================================================== 

(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)
(require (for-syntax syntax/parse))


;; interface: (Klass name (field a) ...+ (method (func_name arg ...) func_body ...) ... )
;; Creating a class macro.
(define-syntax (Klass stx)
  (syntax-parse stx 
    [(_ (~var name id) ((~literal field) (~var a id)) ...+ ((~literal method) (func_name arg ...) func_body ...+) ...)
     (with-syntax ([obj-def (format-id #'name "define-obj-~a" #'name)])
       #'(begin
           ;; internal data repr of the class.
           (struct name (a ...))

           ;; Calling a macro that initializes all the self syntax paramters for the corresponding fields.
           (Klass-skeleton a ...)

           ;; creating an object of a specific class by expanding into defining a struct and defining
           ;; macros for dot operations on fields.
           (define-syntax (obj-def stx)
             (syntax-parse stx 
               [(_ (~var obj id) (val (... ...)))
                #`(begin
                    (define obj (name val (... ...))) ; available at run time.

                    ;; calling define-obj-field macro for all the fields in Klass.
                    #,@(for/list ([prop (syntax->list #'(a ...))])
                         (with-syntax ([prop-name prop]) 
                           #`(define-obj-field obj name prop-name)))

                    ;; creating an obj.func_name definition for each function in the pattern.
                    #,@(for/list ([func (syntax->list #'(func_name ...))]
                                  [args (syntax->list #'(arg ... ...))]
                                  [bodys (syntax->list #'(func_body ... ...))])
                         (with-syntax ([obj.func_name (format-id #'obj "~a.~a" #'obj func)]
                                       [obj.arg  args]
                                       [obj.body bodys])
                           #`(define (obj.func_name obj.arg)
                               (syntax-parameterize (#,@(for/list ([value (syntax->list #'(a ...))])
                                                          (with-syntax  ([obj.val (format-id #'obj
                                                                                             "~a.~a"
                                                                                             #'obj
                                                                                             value)]
                                                                         [r_val (format-id #'obj
                                                                                           "~a.~a"
                                                                                           #'self value)])
                                                            #'[r_val (make-rename-transformer #'obj.val)])))
                                                    obj.body)))))]))))]))


;; creating a macro for obj.field.
(define-syntax (define-obj-field stx)
  (syntax-parse stx
    [(_ (~var obj id) (~var class id) (~var field id))
     (with-syntax ([obj.field (format-id #'obj "~a.~a" #'obj #'field)]
                   [class-field (format-id #'obj "~a-~a" #'class #'field)])
       #'(define-syntax obj.field               ;; IDENTIFIER MACRO.
           (lambda (stx)
             (syntax-case stx ()
               [obj.field (identifier? (syntax obj.field)) (syntax (class-field obj))]))))]))

;; A macro to initialize the syntax-parameters for all the fields in a Klass.
(define-syntax (Klass-skeleton stx)
  (syntax-parse stx 
    [(_ (~var field id) ...+)
     #`(begin
         #,@(for/list ([value (syntax->list #'(field ...))])
              (with-syntax ([self.field (format-id value "~a.~a" #'self value)])
                ;(displayln #'self.field)
                #'(define-syntax-parameter self.field (lambda (stx1) 
                                                        (raise-syntax-error (syntax-e stx1)
                                                                            "cannot be used outside of def"))))))]))


;; =================================================================================================== 

;; Run
(Klass bike (field name) (field no) (method (get-no x) (displayln (+ x self.no))) (method (get-name x) (displayln self.name)))
(define-obj-bike first ("hayabusa" 10))
first.name
(first.get-no 2)
(first.get-name 3)

