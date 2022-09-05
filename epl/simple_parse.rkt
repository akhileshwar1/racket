#lang racket

(define (var-exp datum)
  `(var-exp (,datum)))

(define (lambda-exp var body)
  `(lambda-exp (,var ,body)))

(define (app-exp rator rand)
    `(app-exp (,rator ,rand)))

; parse a lambda expression.
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
         (lambda-exp
           (car (cadr datum))
           (parse-expression (caddr datum)))
         (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum)))))
      (else (display "invalid")))))

(parse-expression '(lambda (x) (inc x)))
