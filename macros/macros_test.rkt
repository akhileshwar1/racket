#lang racket

(require (for-syntax racket/match))
;; Not a good use of suntax transformer. It can be done with a function with ease.
(define-syntax thumbs-up
    (lambda (stx)
      (syntax (display-add))))

(define (display-add)
  (begin (displayln "Hurray")
         (displayln 5)))

(thumbs-up)
(display-add)

;;another use case.
;;what is the use of this transformation when I can directly enter the resulting expression?
;;maybe in cases where we don't know the exact syntax that we want, but we do know the rules that govern its creation.
;;or in cases where syntax isn't just transformed but is also added new pieces.But how will it be different than a function?
;;a function doesn't allow for using the function name in computation. But how is it of any use?
(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

(reverse-me "GO" "you" "Here" values)


;;another use case.
;;it is actually useful here because implementing it with a function will evaluate both the side-effect conditions as they are done at run time. 
;;with JIT compiler racket is able to compile a macro and rewrite the code.
;;So, in a nutshell, what does macro give you?
;;the ability to rewrite stuff in compile time.
;;this ability comes in handy to write new linguistic constructs.
(define-syntax (my-if stx)
  (datum->syntax stx (cons 'if (cdr (syntax->datum stx)))))

(my-if #t (display "true") (display "false"))

(define-syntax (unless stx)
  (match (cdr (syntax->datum stx))
         [(list condition expr)
          (datum->syntax stx `(cond [,condition "nothing"]
                            [else ,expr]))]))

(unless (> 5 4) "YES!")

(define-syntax first-to-last 
  (syntax-rules ()
    [(rotate a b ...) (displayln (list b ... a))]))

(first-to-last 1 2 3 4 5 6)

;;key point: macros are the new syntax. the function from syntax?->syntax? is a transformer. Therefore, identifier macro gives you a syntax that can do more depending on the context. that is the very reason that the syntax for that includes "syntax-case".
;; why does the identifier definition maintain that they can be used without a '(' at their start. TO provide more contexts for sure. 
(define-syntax val
  (make-set!-transformer
    (lambda (stx)
      (syntax-case stx (set!)
                   [val (identifier? (syntax val)) #'(get-val)] ; val as value in a context.
                   [(set! val atom) #'(put-val! (+ atom val))])))); val as the reference in a context, the proc with + calls the macro 'val' again. In a way macro exp                                                                                                       ands to a call containing itself.
                   ;[(add val atom) #'(add val atom)])))) redundant case as it is handled in the first case.

(define-values (get-val put-val!)
    (let ([private-val 0])
      (values (lambda () private-val)
              (lambda (v) (set! private-val v)))))

(set! val 10)
(set! val 20)
val

;;NOw many of these identifier macros, but we dont want to write a seperate definition for each one of them.
;;therefore we write a macro that generates these identifier macros.

(define-syntax-rule (define-get/put-id id get put)
                    (define-syntax id (make-set!-transformer
                                        (lambda (stx)
                                          (syntax-case stx (set!)
                                                       [id (identifier? (syntax id)) #'(get)]
                                                       [(set! id atom) #'(put atom)])))))

(define-get/put-id val2 get-val put-val!) ;returns macro and also creates a binding with a transformer for further expansion. 
(set! val2 10) ;checks the compile time binding and calls the macro
val2
(define-get/put-id val3 get-val put-val!)
(set! val3 100)
val3

;;creating a call by reference macro. 
;;examples: suppose (define-cbr (f a b) (do something))
;;now what do you expand to when you call (f a b) -> (do-f getters setters)
;;what is do-f? It is when you expand the define-cbr -> (define (do-f getters setters) (create a) (create b) (do the rest))
;;the approach is wrong because different fs in define-cbrs will all expand to function with a same name do-f. WHich one to call. 
;;it also says that the call and definition are different macros. the problem is you will have to define the two macros for every possible cbr function. 
;;we want an approach which joins call and definition and works for may instances of cbr functions. 
;;suppose define-cbr macro expands into a macro definition of the form (f a b) using (define-syntax-rule maybe)
;;now this syntax-rule when called would expand into (do-f getters setters), where do-f is a user defined function that uses a and b as identifier macros.
;;this user defined funtion should be available at the compile time, not run time, therefore we need to add a begin clause and use it in define-cbr macro.

(define-syntax r
  (syntax-rules ()
    [(id arg ...)
     "Hurray"]))
(r 1 2 3)

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

;; look at (... ...) this shows that ... is used to define rewriting patterns at compile time. Since, the reqriting at compile time, in this case, is itself a macro definition we introduce one more ... making it (... ...) to make it stand apart from a single ... .
;; single ... pertains to the (define-cbr) macro definition, (... ...) pertains to the (id) macro defintion.
(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...) )
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      () ; explained below...
      body)))

(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
       (gens ...) body)
     (define-for-cbr do-f (id ...)
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))


(define-cbr (f a b)
  (swap a b))
 
(let ([x 1] [y 2])
  (f x y)
  (list x y))

;; MY OWN MACRO SHORTHAND DEFINITION.
;; ERROR 
#;(define-syntax-rule (macro (id arg arg1) body)
                    (define-syntax-rule (id actual actual1) ;we use actual instead of arg because we are defining a pattern variable here, Reusing arg would have taken its value.
                                        body)) ;; the body is unbound here in this macro definition. ERROR.
;; using nested syntax case.
(define-syntax (macro stx)
  (syntax-case stx ()
               [(macro (id arg ...) body0 body ...) (syntax-case #'define-syntax-rule
                                                                 ()
                                                                 [def #'(def (id arg ...) body0 body ...)])])) ; KEY POINT: you cant use outer pattern variables in the inner pattern variables as they themselves are new.

;; using recursive syntax case.
(define-syntax (macro-v2 stx)
  (syntax-case stx ()
               [(macro-v2 (id arg ...) body0 body ...) #'(macro-v2 define-syntax-rule (id arg ...) body0 body ...)]
               [(macro-v2 new-syntax (id arg ...) body0 body ...) #'(new-syntax (id arg ...) body0 body ...)]))



(macro-v2 (swapp actual actual1)
  (let ([tmp actual])
    (set! actual actual1)
    (set! actual1 tmp)))

(let ([x 3] [y 4])
     (swapp x y)
     (list x y))




