#lang racket

(provide
  ; A class macro of the form (Klass name (field a)
  ;                                       ...+
  ;                                       (method (func_name arg ...)
  ;                                       func_body
  ;                                       ...)
  ;                                       ...)
  ; where field is the class data, and methods correspond to the functions of the class.


  ; Also implicitly provides a define object form with the interface:
  ; (define-obj-{class} (~var obj id) (field values))
  ; where class refers to the class of the object,
  ; obj is the object name, and values are the initial data values of the object.
  Klass)

; --------------------------------------------------------------------------------------------------- 
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)
(require (for-syntax syntax/parse))
(require (for-syntax "distinct-ids-sc.rkt"))
(require rackunit)
(require syntax/macro-testing)
(require rackunit/text-ui)	
; ---------------------------------------------------------------------------------------------------

; Implementation
; Approach: To enable the dot operations to access class data, I have stored a class as a struct
;           with fields[Line 50] and defined (obj.field)[line 98] for each field.
;           Secondly, to give an access to the functions of a particular object
;           I have defined a function for each (obj.method)[Line 77].

; Creating a class macro.
(define-syntax (Klass stx)
  (syntax-parse stx 
    [(_ (~var name id) ((~literal field) (~var a id)) 
        ...+
        ((~literal method) (func_name arg ...) func_body ...+)
        ...)

     ; calling distinct-ids syntax-class to see whether every field and func is unique.
     #:with (~var fields distinct-ids) #'(a ...)
     #:with (~var funcs distinct-ids) #'(func_name ...)

     (with-syntax ([obj-def (format-id #'name "define-obj-~a" #'name)])
       #'(begin
           ; internal data repr of the class.
           (struct name (a ...))

           ; Calling a macro that initializes all the self syntax paramters for the 
           ; corresponding fields.
           (Klass-skeleton a ...)

           ; creating an object of a specific class by expanding into defining a struct and
           ; macros for dot operations on fields.
           (define-syntax (obj-def stx)
             (syntax-parse stx 
               [(_ (~var obj id) (val (... ...)))
                #`(begin
                    (define obj (name val (... ...))) ; available at run time.

                    ; calling define-obj-field macro for all the fields in Klass.
                    #,@(for/list ([prop (syntax->list #'(a ...))])
                         (with-syntax ([prop-name prop]) 
                           #`(define-obj-field obj name prop-name)))

                    ; creating an obj.func_name definition for each function in the pattern.
                    #,@(for/list ([func (syntax->list #'(func_name ...))]
                                  [args (syntax->list #'((arg ...) ...))]
                                  [bodys (syntax->list #'((func_body ...) ...))])
                         (with-syntax ([obj.func_name (format-id #'obj "~a.~a" #'obj func)]
                                       [obj.arg  args]
                                       [obj.body bodys])
                           #`(define #,(cons #'obj.func_name #'obj.arg)
                               (syntax-parameterize
                                 (#,@(for/list ([value (syntax->list #'(a ...))])
                                       (with-syntax 
                                         ([obj.val (format-id #'obj "~a.~a" #'obj value)]
                                          [r_val (format-id #'obj "~a.~a" #'self value)])
                                         #'[r_val (make-rename-transformer #'obj.val)])))
                                 #,@(syntax->list #'obj.body))))))]))))]))


; Helper macro to create obj.field macro.
(define-syntax (define-obj-field stx)
  (syntax-parse stx
    [(_ (~var obj id) (~var class id) (~var field id))
     (with-syntax ([obj.field (format-id #'obj "~a.~a" #'obj #'field)]
                   [class-field (format-id #'obj "~a-~a" #'class #'field)])
       #'(define-syntax obj.field               ;; IDENTIFIER MACRO.
           (lambda (stx)
             (syntax-case stx ()
               [obj.field (identifier? (syntax obj.field)) (syntax (class-field obj))]))))]))


; Helper macro to initialize the syntax-parameters for all the fields in a Klass.
(define-syntax (Klass-skeleton stx)
  (syntax-parse stx 
    [(_ (~var field id) ...+)
     #`(begin
         #,@(for/list ([value (syntax->list #'(field ...))])
              (with-syntax ([self.field (format-id value "~a.~a" #'self value)])
                ;(displayln #'self.field)
                #'(define-syntax-parameter self.field (lambda (stx1) 
                                                        (raise-syntax-error (syntax-e stx1)
                                                                            "cannot be used outside
                                                                            of def"))))))]))



; ---------------------------------------------------------------------------------------------------

; Tests
(define tests
  (test-suite
    "All tests for Klass"
    ; test-case for a sample class called bike.
    (test-begin
      "for sample class bike"
      (Klass bike 
             (field name)
             (field no)
             (method (get-no x y)
                     (displayln "computing")
                     (+ (+ x y) self.no))
             (method (get-name x)
                     self.name))

      (define-obj-bike first ("hayabusa" 10))
      (check-equal? first.name "hayabusa")
      (check-equal? (first.get-no 2 3) 15)
      (check-equal? (first.get-name 3) "hayabusa"))

    ; tests for all compile time errors.
    (check-exn #rx"duplicate identifier found" (lambda () 
                                                 (convert-compile-time-error 
                                                   (Klass bike
                                                          (field name)
                                                          (field name)))))))





(run-tests tests)             

