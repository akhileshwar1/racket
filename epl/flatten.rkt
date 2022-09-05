#lang racket

; listofanything -> list of singular elements.
; takeout all the nested parens and return a flat list in the same order.
(define (flatten lst)
  (flatten-into lst '()))

; creates a new list called n list with parens out.
; listofanythging -> list of singles.
(define (flatten-into lst nlst)
  (cond [(empty? lst) nlst]
        [(or (symbol? (car lst)) (number? (car lst))) (flatten-into (cdr lst) 
                                                               (cons (car lst)
                                                                     nlst))]
        [else (flatten-into (cdr lst) (flatten-into (car lst) nlst))]))


(flatten '((a b) c))

