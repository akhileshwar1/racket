#lang racket
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require macro-debugger/stepper)
(require macro-debugger/expand)
(require macro-debugger/stepper-text)
(require racket/stxparam)
(require (for-syntax syntax/parse))

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


;; quasi quoting and unquote example.
;; build the expr--(8(7(6(5...0))))--programmatically.
(define (recur n)
  (cond [(zero? n) 0]
        [else (list n (recur (sub1 n)))]))

;; same thing.
;; what if we want let before all this.
(define (deep n)
    (cond
      [(zero? n) 0]
      [else
       (quasiquote (let ((unquote n) (unquote (deep (- n 1))))))]))  ;; the let is stripped of its context here. 

(deep 8)


;(let ([it 0])
;  (define-it 5)  ; a macro has its own scope.  
;  (+ it it)) 

(displayln "HEERE")

;; KEY point: The only explanation that I can think of is that a macro is only meaningful in a context. It can be genralized about any piece of code as there's a;ways a top level context.

;; how will you make define-it work outside the scope. ?
;; "datum->syntax converts a piece of datum into a syntax object. A syntax object is simply piece of Racket data with additional scope information attached to it. Therefore, datum->syntax takes two arguments. The second is just the data; in this case new name you want to bind. The first is a syntax object that currently has the scope that you want the new name to be bound inside of; in this case, we want the new name to be bound in the same scope as the expression it is bound to, in this case #'e."

(define-syntax (define-it-v2 stx)
  (syntax-case stx ()
  [(_ e)
  (with-syntax ([the-new-name (datum->syntax #'e 'it)]) ; creating the new syntax as well as giving it a scope same as e.
  #'(define the-new-name e))]))

(let ([expr 5])
     (define-it-v2 expr)
     (+ it it))

;; using syntax-parameters to do it more beautifully.
#|(define-syntax-parameter it
                         (lambda (stx)
                              (raise-syntax-error #f "Illegal outside with-it" stx)))|#

#|(define-syntax (with-it stx)
  (syntax-case stx ()
               [(with-it e body ...)
                #'(let ([this-it e])
                (syntax-parameterize ([it (lambda(stx) #'this-it)])
                                     body ...))]))

(with-it 5 (+ 1 it))|#

;; so the thing is that with-syntax lets us introduce new syntax and also breaks scope hygiene rules.
;; syntax-parameter keeps the hygiene rules, but helps us define new syntax in a way that breaking the rules can signal a custom message.

(define-syntax forever
    (syntax-rules ()
        [(forever body ...)
            (call/cc (lambda (abort)
                (let loop () body ... (loop))))]))



(define-syntax (forever-v2 stx)
(syntax-case stx ()
    [(forever body ...)
        (with-syntax ([abort (datum->syntax #'forever 'abort)])
        #'(call/cc (lambda (abort)
        (let loop () body ... (loop)))))]))

#;(let ([i 0])
  ((forever-v2 (set! i (+ i 1)) (display i)) (equal? i 5)))

(define-syntax (while stx)
  (syntax-case stx ()
               [(while test body ...)
                #'(let loop () (if test
                                   (begin body ... (loop))
                                   (display "done")))]))

(let ([i 0])
  (while (< i 5) (displayln i) (set! i (+ i 1))))


;; my implementation of syntax->datum using syntax-e and datum->syntax.
(define (my-syntax->datum stx)
  (define e (syntax-e stx))
  (cond [(empty? e) e]
        [(list? e) (cons (my-syntax->datum (car e)) (my-syntax->datum (datum->syntax stx (cdr e))))]  ; why datum->syntax and not #'.
        [else e]))

(my-syntax->datum #'(define akhil 10))


;; IN the previous example, datum->syntax was used as it evaulates it args, unlike #'.
;; THE Word evaulation suggests that Racket sees two things code and data. The difference is that code comes with a context to evaluate. 
;; IN many of my macros, I have used #' for creating syntax with no context arg, but they worked fine. Perhaps because it was being expanded in a context that had there references.
;; but with syntax I had to use datum->syntax because it was a new syntax that had no reference in the context it was being expanded into. 


;; anaphoric if.
(define-syntax-rule (aif test succ-expr fail-expr)
                    (let ([that (+ test 1)])
                         (if that 
                             (begin succ-expr (displayln that))  ;; prints [10 11] because succ-expr uses that from the outer scope, whereas the other one uses that from                                                                  macro's scope.
                             fail-expr)))

(let ([that 10])
  (aif that (displayln that) (display 0)))  

;; GAME CHANGER: Every code or syntax object has a context(where all the bindings are, in case of a list) and a "lexical scope information of its source",                 where source is the parent code containing that particular syntax.

;; REDUNDANT anaphoric if, but it still makes you define thatt outside.
(define-syntax (aif-v2 stx)
  (syntax-case stx ()
               [(aif-v2 test succ-expr fail-expr)
                (with-syntax ([thatt (datum->syntax #'test #'thatt)]) ;; this binding's lexical scope changes to the macro call area i.e thatt can be exported outside 
                             #'(let ([thatt (+ 1 test)])
                                    (if thatt 
                                     (begin succ-expr (displayln thatt))  
                                      fail-expr)))]))

(let ([thatt (+ 5 5)])
  (aif-v2 thatt (displayln thatt) (display 0))
  ) 

;; correct anaphoric if lets you do this: (aif 10 (displayln it) (void))-->10.
(define-syntax (aif-v3 stx)
  (syntax-case stx ()
               [(aif-v2 test succ-expr fail-expr)
                (with-syntax ([itt (datum->syntax #'test #'itt)]) ;; this binding's lexical scope changes to the macro call area i.e thatt can be exported outside 
                             #'(let ([itt test])
                                    (if itt 
                                     (begin succ-expr (displayln itt))  
                                      fail-expr)))]))
;(aif-v3 10 (displayln itt) (void))
        ; the itt from the inside could have been used here.) ;; throws an error because before the expansion (display itt) already had a context and a scope where the binding wasn't defined.
                                   ;; it thorws the error after expansion, but the particular syntax object wasn't bound while calling. 



;; So how do we go past this? One way is to get a binding before, but that will be used even after expansion.
;; Lets check.

(let ([itt 0])
  (aif-v3 10 (displayln itt) (void))
  itt) ; output: 0, i.e it uses the earlier binding/ Why?  the bindings and scopes are determined before expansion for the other syntax/
       ; but how come line 226 worked with it?

;; it seems like syntax-parameter is the only way.
;; the problem: there is a binding at a call time that was supposed to be provided during expansion. it doesn't work because the expr doesn;t come after expansion. like the case of define-it-v2.
;; that is binding comes after expansion, but is needed at call time. 
;; with syntax-parameter you define a macro in the use site scope, so you can use it there. And then in the expansion phase you redefine the use site name to something inside the macro's scope.

(define-syntax-parameter thati
                         (lambda (stx)
                           (raise-syntax-error (syntax-e stx) "cannot be used outside of aif")))

(define-syntax (aif-v4 stx)
  (syntax-case stx ()
               [(_ test succ-expr fail-expr)
                #'(let ([tmp test])
                       (syntax-parameterize ([thati (make-rename-transformer #'tmp)])
                                            (if tmp
                                                succ-expr
                                                fail-expr)))]))

(displayln "parameterize")
(aif-v4 10 (displayln thati) (void))

(define-syntax (ex stx)
  #'5)

(ex 1)

(define-syntax EOF
  (λ (stx) (raise-syntax-error #f "Illegal outside with-ignored" stx)))
;; literals example.
(define-syntax (with-ignored stx)
  (syntax-parse stx
    #:literals (EOF)
    #:track-literals
    [(_ before ... EOF after ...)
     #'(let () before ...)]))

(with-ignored
  (define x 6)
  (+ x x)
  EOF
  x y z
  (/ 1 0))
