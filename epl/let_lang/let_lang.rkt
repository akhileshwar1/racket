#lang eopl

(provide (all-defined-out))
; ------------------------------------------------------------------------------

; the sanner spec.
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-(" expression "," expression ")") diff-exp)
    (expression ("zero?(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))


; creating the scanner and parser.
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))


(sllgen:make-define-datatypes scanner-spec grammar)

; ------------------------------------------------------------------------------
;; ENV REPRESENTATION.

; Grammar for env repr:
;  Env ::= '()
;        |  '((var val) env)



; to check whether its an empty-env.
; list -> boolean.
(define (empty-env? env)
  (if (eqv? '() env)
    #t
    #f))

; to return an empty-env.
;  nothing -> list.
(define (empty-env)
  '())

; to extend the environment with the particular binding.
; env -> env.
(define (extend-env var val env)
  (cons (cons var val) env))

; to extend many vars at the same time.
; list of vars, vals -> new env.
(define (extend-env-lst vars vals env)
  (cond [(eqv? (cdr vars) '()) 
         (extend-env (car vars) (car vals) env)]
        [else 
          (extend-env-lst (cdr vars) (cdr vals)
                          (extend-env (car vars) (car vals) env))]))

; to get the value of the variable in env.
; var, env -> value or error.
(define (apply-env env var)
  (cond [(eqv? env '()) (display "no binding available")]
        [(eq? (caar env) var) (cdar env)]
        [else (apply-env (cdr env) var)]))

; -----------------------------------------------------------------------------

; define datatype for expressed values.
(define-datatype expval expval?
                 (num-val
                   (num number?))
                 (bool-val
                   (bool boolean?))
                 (proc-val
                   (proc proc?)))


; expval->num : ExpVal → Int
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (display val)))))


; expval->bool : ExpVal → Bool
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (display val)))))

; expval -> proc
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (else (display val)))))

(define expval->
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (bool-val (bool) bool)
           (proc-val (proc) proc)
           (else (display val)))))

; usage: (init-env) = [i= 1  ,v= 5  ,x= 10  ]
(define init-env
  (lambda ()
    (extend-env
      'i (num-val 1)
      (extend-env
        'v (num-val 5)
        (extend-env
          'x (num-val 10)
          (empty-env))))))

; ------------------------------------------------------------------------------

; defining the data type for procedures.


; constructor procedure.
; var, body, env -> proc-val.
(define procedure
  (lambda (vars body env)
    (lambda (vals)
      (value-of body (extend-env-lst vars vals env)))))

; observor procedure.
; procedure => boolean.
(define proc?
  (lambda (val)
    (procedure? val)))

; apply procedure.
; proc, expval -> expval
(define apply-procedure
  (lambda (proc1 vals)
    (proc1 vals)))

; ------------------------------------------------------------------------------

;; THE CORE INTERPRETER.

; run : String → ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

; value-of-program : Program → ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

; internal function to interpret a list of expressed values.
(define (value-of-list lst env)
  (cond [(eq? '() lst) 
         '()]
        [else (cons ;(expval-> 
                      (value-of (car lst) env)
                    (value-of-list (cdr lst) env))]))


; value-of : Exp × Env → ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env env var))
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                             (- num1 num2)))))
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                            (bool-val #t)
                            (bool-val #f)))))
           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                       (value-of exp2 env)
                       (value-of exp3 env))))

           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))
           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (vals (value-of-list rand env)))
                       (apply-procedure proc vals))))))




