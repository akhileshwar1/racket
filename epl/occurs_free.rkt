#lang racket

(define (in-args? s exp)
  (if (member s exp)
      #t
      #f))

(define (in-lambda? s exp)
  (cond [(and (not (in-args? s (car exp))) (occurs-free? s (cdr exp))) #t]
        [else #f]))

(define (lambda? exp)
  (eq? (car exp) 'lambda))

(define (occurs-free? s exp)
  (cond [(symbol? exp) (eq? s exp)]
        [(lambda? exp) (in-lambda? s (cdr exp))]
        [else (or (occurs-free? s (first exp))
                   (occurs-free? s (second exp)))]))

(occurs-free? 'x 'x)
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))


