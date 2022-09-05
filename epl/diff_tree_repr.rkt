#lang racket

; Grammar:
; diff-tree ::= (one)
;           | (diff diff-tree diff-tree)


; Integer n ::=  (one) , n = 1
;             |  (diff n1 n2) , n = n1 - n2
;
; IMPLEMENTATION:
;


; Integer -> Integer
; returns the next integer in the repr.

(define (successor n)
  (cond [(equal? '(one) n) '(diff (one) (diff (diff (one) (one)) (one)))]
        [else `(diff ,(successor (cadr n)) ,(caddr n))]))

(successor '(diff (one) (one)))
