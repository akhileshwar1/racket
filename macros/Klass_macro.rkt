#lang racket
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)

;; interface: (Klass name (a b ...) (methods ((func_name arg ...) func_body ...) ... ))
(define-syntax (Klass stx)
  (syntax-case stx ()
               [(_ name (a b ...) (((func_name arg) func_body) ...))
                (with-syntax ([fields (format-id #'name "~a-~a" #'name #'values)]
                              [obj_def (format-id #'name "define-obj-~a" #'name)])

                #'(begin
                    (define fields '(a b ...)) ; defined and available  at run time.
                    (struct name (a b ...))
            
                ;; creating an object of a specific class by expanding into defining a struct and defining macros for   dot operations on fields.
                (define-syntax (obj_def stx)
                  (syntax-case stx ()
                               [(_ obj (val (... ...)))
                                #`(begin
                                    (define obj (name val (... ...))) ; available at run time.

                                    ;; calling define-obj-field macro for all the fields in Klass.
                                    #,@(for/list ([prop (syntax->list #'(a b ...))])
                                                 (with-syntax ([prop-name prop]) 
                                                 #`(define-obj-field obj name prop-name))) ; macros defined at compile time. Should be Available.
                                    
                                    
                                    ;; creating an obj.func_name definition for each function in the pattern.
                                    #,@(for/list ([func_def (syntax->list #'(((func_name arg) func_body) ...))])
                                                 (with-syntax ([obj.func_name (format-id #'obj "~a.~a" #'obj #`#,(caar (syntax->datum func_def)))]
                                                               [obj.arg  #`#,(cadar (syntax->datum func_def))]
                                                               [obj.body #`#,(cdr (syntax->datum func_def))])
                                                 #`(define (obj.func_name obj.arg)
                                                     (syntax-parameterize (#,@(for/list ([value (syntax->list #'(a b ...))])
                                                                                       (with-syntax  ([obj.val (format-id #'obj "~a.~a" #'obj value)] [r_val (format-id #'obj "~a" value)])
                                                                                        #`[r_val (make-rename-transformer #'obj.val)])))
                                                                          obj.body  
                                                                          )))
                                                 ))]))))]))


;; creating a macro for obj.field.
(define-syntax (define-obj-field stx)
  (syntax-case stx ()
               [(_ obj class field)
                (with-syntax ([obj.field (format-id #'obj "~a.~a" #'obj #'field)]
                              [class-field (format-id #'obj "~a-~a" #'class #'field)]
                              )
                             #'(define-syntax-rule (obj.field)
                                                   (class-field obj))
                                 #;(define-syntax-parameter self.field 
                                 (lambda (stx1)
                                   (raise-syntax-error (syntax-e stx1) "cannot be used outside of object definition")))
                                  )]))


#;(define-syntax one 1)

;; A macro to initialize the syntax-parameters for all the fields in a Klass.
(define-syntax (Klass-skeleton stx)
  (syntax-case stx ()
               [(_ field ...)
                #`(begin
                    #,@(for/list ([val (syntax->list #'(field ...))])
                                 (with-syntax ([name val]
                                               )
                                              #'(define-syntax-parameter name 
                                                                                     #;(make-rename-transformer #'one)
                                 (lambda (stx1)
                                   (raise-syntax-error (syntax-e stx1) "cannot be used outside of object definition"))))))]))

(Klass-skeleton name no)
(Klass bike (name no) (((get-no x) (display (no))) ((get-name x) (display (name)))))
(define-obj-bike first ("hayabusa" 10))
(first.name)
(first.get-no 2)
(first.get-name 3)

