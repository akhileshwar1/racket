#lang racket

;; =================================================================================================== 
(require loop)
(require (for-syntax racket/match))
(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require macro-debugger/stepper)
(require macro-debugger/expand)
(require macro-debugger/stepper-text)
(require (for-syntax syntax/parse))
(require (for-syntax "distinct-ids-sc.rkt"))
(require (for-syntax "optionally-postfixed-sc.rkt"))
;; creating a struct macro.
;; (struct name (a b ...)) -> (define a macro with 2 cases)
;; first case: (name a b ...) -> (list a b ...)
;; second case: (name-a/b ... inst) ->  (list-ref lst no)
(define-syntax (my-struct stx)
  (syntax-case stx ()
    [(my-struct name (a b ...))
     (syntax (define-syntax (name stx1)
               (syntax-case stx1 ()
                 [(name x y (... ...)) (syntax (list 'name x y (... ...)))])))]))


;; the second case wasn't possible with the path I was on. 
;; the correct way is to define three functions instead of a macro.
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...)) 
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ;; Define the predicate function.
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ;; Define the accessor function for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))


;; --------------------------------------------------------------------------------------------------- 

;; creating a macro for for/list.
;; (for/list ([var lst]) body0 body ...) -> (define a function with params as (list)
;; that accumulates in a list and call it as well.
;; example: (for/list ([var '(1 2 3)]) (+ var 1))
(define-syntax (my-for/list stx)
  (syntax-case stx ()
    [(_ ([var lst]) body0 body ...)
     #'(begin
         (define (for lt)
           (cond [(empty? lt) '()]
                 [else (let* ([var (first lt)]
                              [result (begin body0 body ...)])
                         (cons result (for (rest lt))))]))
         ;; Function call.
         (for lst))]))

;; verification.
(my-for/list ([v '(1 2 3)]) (display "hey") (+ v 1))
(for/list ([v '(1 2 3)]) (display "hey") (+ v 1))

;; for/list for multiple lists at once.
(define-syntax (mul-for/list stx)
  (syntax-parse stx
    [(_ ([elem-name a-list] ...) computation)
     #'(letrec ([iteration
                  (λ (elem-name ...) computation)]
                [looping
                  (λ (elem-name ...)
                     (cond
                       [(or (empty? elem-name) ...) '()]
                       [else
                         (cons (iteration (first elem-name) ...)
                               (looping (rest elem-name) ...))]))])
         (looping a-list ...))]))


;;Test
(mul-for/list ([a '(1 2 3)]
                  [b '(3 4 5)])
                 (+ a b))
;; --------------------------------------------------------------------------------------------------- 

;; creating a macro for classic for loop.
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

;; run.
(my-for and ([v '(1 2 x)]) (display "hey") (number? v))



;; --------------------------------------------------------------------------------------------------- 

;; creating a standard while loop.
;; interface: (while test body ...)
(define-syntax (while stx)
  (syntax-case stx ()
    [(while test body ...)
     #'(let loop () (if test
                      (begin body ... (loop))
                      (display "done")))]))

;; run
(let ([i 0])
  (while (< i 5) (displayln i) (set! i (+ i 1)))) 

;; --------------------------------------------------------------------------------------------------- 

(define-syntax (define-simple-macro stx)
  (syntax-parse stx
    [(_  (name arg ...) body)
     #'(define-syntax (name stx1)
         (syntax-parse stx1
           [(name arg ...)
            #'body]))]))

;;verify
(define-simple-macro (test a b)
                     (if a
                       a
                       b))

(test 2 #f)

;; --------------------------------------------------------------------------------------------------- 
;; testing syntax-parse
;; Q: if begin disappears as said below, then why does kandi throw an error in use site context. Maybe because when it was introduced the scope contained the macro scope.
(define-syntax (define-hello stx)
  (syntax-parse stx
    [(_ (~var a id) b)
     #'(begin
         (define a b)
         (define kandi 20))]))

(define-hello d "world")

;; Key point: the begin disappears, the definitions are spliced and added in the expression context.
(define-syntax (define-hello* stx)
  (syntax-parse stx
    [(_ (~var x id) ...+)
     #'(begin
         (define x "world") ...)]))

(define-hello* akhil kandi)
akhil
kandi

(define-syntax (define-hello*-v2 stx)
  (syntax-parse stx
    [(_ (~var x id) ...)
     #'(define-values (x ...)
        (values (begin 'x  "world") ...))])) ;; works because begin is instantiated for every identifier in x seq, and begin returns world only.

(define-hello*-v2 world good bye)
world
good

;; prefix literal
(define-syntax (define-hello*-v3 stx)
  (syntax-parse stx
    [(_ (~optional ((~literal pre) (~var p str))) (~var x id) ...+)
     #'(begin
         (define x (string-append (~? p "") "world")) ...)]))

(define-hello*-v3 (pre "hello") ak ka)
ak
(define-hello*-v3 akk kaa)
akk

;;  ---------------------------------------------------------------------------------------------------  
;;  creating a recursive macro to define variables with a postfix.
(define-syntax (define-hello-post stx)
  (syntax-parse stx
    [(_ ((~var p id) (~var value str)) others ...) #'(begin (define p (string-append "world" value))
                                                            (define-hello-post others ...))]
    [(_ (~var p id) others ...) #'(begin (define p "world")
                                         (define-hello-post others ...))]
    [(_) #'(begin)]))

(define-hello-post g [f ", hello"] h)
(list g f h)


;; version 1 of define-hello-post using more functionality of syntax-parse.
(define-syntax (define-hello-post-v1 stx)
  (syntax-parse stx
    [(_ x-or-x-post ...)
    (define xs+ps
      (for/list ([one (syntax-e #'(x-or-x-post ...))])
        (syntax-parse one
          [(~var x id) (list #'x #'"")]
          [((~var x id) (~var p str)) (list #'x #'p)])))

    #`(begin
        #,@(for/list ([y+q xs+ps])
             (define prefix (first y+q))
             (define postfix (second y+q))
             #`(define #,prefix (string-append "world" #,postfix))))]))


(define-hello-post-v1 s [t ", hello"] r)
(list s t r)

;; creating a version two with a compile time function and #:with.
(define-for-syntax (fill-in x-or-x-post)
        (syntax-parse x-or-x-post
          [(~var x id) (list #'x #'"")]
          [((~var x id) (~var p str)) (list #'x #'p)]))

(define-syntax (define-hello-post-v2 stx)
  (syntax-parse stx
    [(_ x-or-x-post ...)
     #:with ((x p) ...) (map fill-in (syntax-e #'(x-or-x-post ...)))
     #'(begin (define x (string-append "world" p)) ...)]))

(define-hello-post-v2 u [b ", hello"] c)
(list u b c)

;; checking duplicates in the use site.
(define-syntax (define-hello-post-protected stx)
  (syntax-parse stx
    [(_ x-or-x-post ...)
     #:with ((x p) ...) (map fill-in (syntax-e #'(x-or-x-post ...)))
     #:fail-when (check-duplicates (syntax-e #'(x ...)) free-identifier=?)
                 "duplicate identifiers at define-hello-post-protected"
     #'(begin (define x (string-append "world" p)) ...)]))

;(define-hello-post-protected v [n ", hello"] n)

(define-syntax (define-hello-post-v3 stx)
  (syntax-parse stx
    [(_ (~optional ((~literal post) (~var s str))). xs)
     #:declare xs (optionally-postfixed (if (attribute s) #'s #'""))
     #:with (x ...) #'xs.ids*
     #:with (p ...) #'xs.post*
     #'(begin (define x (string-append "world" p)) ...)]))


(define-hello-post-v3 (post "diff") z [v ", hello"] n)
(list z v n)
