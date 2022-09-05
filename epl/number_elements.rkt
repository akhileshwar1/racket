#lang racket



(define (number-element-with n element)
  (cons n element))

(define (number-elements-from n lst)
  (if (empty? lst)
   '() 
    (cons (number-element-with n (car lst)) (number-elements (+ 1 n) (cdr lst)))))

(number-elements-from 0 '(a b c d))
