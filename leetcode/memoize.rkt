#lang racket

(define (heavy-comp)
  (apply + (build-list 5000000 values)))

(define (suspended-heavy-comp)
  (lambda ()
    (heavy-comp)))

(define (memoize func)

  (define run? #f)
  (define hidden #f)
 
  (lambda ()
       (cond [run? hidden]
          [else (set! hidden (func))
                (set! run? #t)
                hidden])))

(define m1 (memoize (suspended-heavy-comp)))
(define m2 (memoize (suspended-heavy-comp)))
(m1)
(m2)
(m1)
(eq? m1 m2)
(equal? m1 m2)
