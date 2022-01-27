#lang racket
(define upper 0)
(define lower 0)
(define (start n m)
  (set! upper (max n m))
  (set! lower (min n m))
  (guess))


(define akhil 100)

(define (guess)
  (quotient (+ upper lower) 2))

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

(define (bigger)
  (set! lower (max upper (add1 (guess))))
  (guess))
