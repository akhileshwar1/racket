#lang racket
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)

(define-syntax (my-for stx)
  (syntax-case stx ()
               [(_ func ([var lst]) body0 body ...)
                   #'(begin
                       (define (for lt)
                          (cond [(empty? lt) (void)]
                                [else (let* ([var (first lt)]
                                             [result (begin body0 body ...)])
                                           (func result (for (rest lt))))]))
                        ;; Function call.
                        (for lst))]))




;; interface: (Klass name (a b ...) (methods ((func_name arg ...) func_body ...) ... ))
(define-syntax (Klass stx)
  (syntax-case stx ()
               [(_ name (a b ...) ((func_name arg ...) func_body ...))
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
                                    #,@(for/list ([prop (syntax->list #'(a b ...))])
                                                 (with-syntax ([prop-name prop]) ; at run time in compile time
                                                 #`(define-obj-field obj name prop-name))) ; macros defined at compile time. Should be Available.
                                    
                                    
                                    ;; creating an obj.func_name definition for each function in the pattern.
                                    ;#,@(for/list ([func (syntax->list #'((func_name (arg ...) func_body ...) ...))])
                                                 #,(with-syntax ([obj.func_name (format-id #'obj "~a.~a" #'obj #'func_name)]) ; at run time in compile time
                                                 #`(define (obj.func_name arg ...)
                                                     (syntax-parameterize (#,@(for/list ([value (syntax->list #'(a b ...))])
                                                                                       (with-syntax  ([obj.val (format-id #'obj "~a.~a" #'obj value)] [r_val (format-id #'obj "~a" value)])
                                                                                        #`[r_val (make-rename-transformer #'obj.val)])))
                                                                          func_body ...  
                                                                          )))
                                                 )]))))]))




                

;; creating a macro for obj.field.
(define-syntax (define-obj-field stx)
  (syntax-case stx ()
               [(_ obj class field)
                (with-syntax ([obj.field (format-id #'obj "~a.~a" #'obj #'field)]
                              [class-field (format-id #'obj "~a-~a" #'class #'field)]
                              [self.field (format-id #'obj "~a.~a" #'self #'field)])
                             #'(define-syntax-rule (obj.field)
                                                   (class-field obj))
                                 #;(define-syntax-parameter self.field 
                                 (lambda (stx1)
                                   (raise-syntax-error (syntax-e stx1) "cannot be used outside of object definition")))
                                  )]))


(define-syntax one 1)
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
(Klass bike (name no) ((get) (display (no))))
(define-obj-bike first ("hayabusa" 10))
(first.name)
(first.get)

#;(define-syntax (methods stx)
  (syntax-case stx ()
               [(_ (((func_name arg ...) body ...) ...))
                #`(begin
                    (define lst (#,@(for/list ([func_def (syntax->list #'(((func_name arg ...) body ...) ...))])
                                 (let ([func (car (syntax->datum func_def))])
                                      (display func)
                                      func)))))]))

;(methods (((get) (display 1)) ((gett) (display 2))))
#;(define-syntax (unlist stx)
  (syntax-case stx ()
               [(_ lst)
                #'(apply + lst)]))

;(unlist '(1 2 3))

;; the question that i HAVE IS that how is the code interpreted by the compiler. 
;; When I work with macros it expands into a syntax object, which is nothing but a raw exp with additional exp. WHERE IS THIS ADDITIONAL exp stored. 
;; clearly this additional exp is not stored in the list which contains code. Therefore it must be in the environment. 
;; Therefore the code here is nothing but syntax object, which is a list + env info/ Or list interpreted in a context.
;; why is #` includes #, for unquoting, I think it has to do with a new symbol. Nothing extra is being done here. 
;; why did with-syntax require #'obj.val. FIrstly I was in a template, where i could easily substitute the variables but I created a syntax gave it as an input to make-rename-transformer. It is because syntax-transformer takes syntax as an input. 
;; Key point is that #' or #` does not apply to all the elements inside. It takes a thing as a whole and turns the whole into syntax, not the individual parts. 
;; but when you do syntax-list, it returns a list of syntaxes. Were they elements syntaxes as well. Does syntax turn all the elemets into syntax? 
