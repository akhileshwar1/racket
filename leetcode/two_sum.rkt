#lang racket

(define true-lst '(2 7 11 15))

(define (two-sum lst target)
  (define rem (- target (first lst)))
  (define rem-lst (remove (first lst) true-lst))
  (cond [(empty? lst) #f]
        [(> target (sum lst)) #f]
        [(> (first lst) target) #f]
        [(equal? target (first lst)) (index-of true-lst (first lst))]
        [else (or (if (member? rem rem-lst)
                      (list (index-of true-lst (first lst)) (member? rem rem-lst))
                      #f)
                  (two-sum (rest lst) target))]))

(define (sum lst)
  (cond [(empty? lst) 0]
        [else (+ (first lst) (sum (rest lst)))]))

(define (member? item lst)
  (cond [(empty? lst) #f]
        [else (or (if (equal? item (first lst))
                      (index-of true-lst item)
                      #f)
                  (member? item (rest lst)))]))


(define (two-sum-for lst target)
  (define true-lst (cons-indexes lst))
  (for*/or ([i true-lst]
              [j (remove i true-lst)]
              #:when (equal? (+ (second i) (second j)) target))
             (list (first i) (first j))))

(define (cons-indexes lst)
  (define len (length lst))
  (for/list ([i lst]
             [j (in-range len)])
            (list j i)))

#;(two-sum '(2 7 11 15) 9)
(two-sum-for '(2 7 11 15) 9)
#;(cons-indexes '(1 2 3))
