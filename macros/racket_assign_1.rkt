#lang racket


;;Q: How does letrec behave keeping in mind that it originates from let core logic. 


;;Expression	 	=	 	...
;; 	 	|	 	(loop Variable0 ((Variable1 Expression1) ...) Expression0 ...)
;; creating a loop macro which 



#;(let proc-id ([arg-id init-expr] ...)
  body ...+)
;; creating a named-let form.
(define-syntax (named-let stx)
  (syntax-case stx ()
               [(_ proc-id ([arg-id init-expr] ...) body ...)
                #'(letrec ([proc-id (lambda (arg-id ...) body ...)])
                          (proc-id init-expr ...))]))

(named-let recur ([x 2] [y 3]) 
                 (cond 
                   [(zero? x) y]
                   [else (recur (- x 1) (- y 1))]))
