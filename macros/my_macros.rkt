#lang racket

;; =================================================================================================== 

(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require macro-debugger/stepper)
(require macro-debugger/expand)
(require macro-debugger/stepper-text)

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

