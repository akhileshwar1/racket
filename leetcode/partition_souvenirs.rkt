#lang racket

(define (can W lst)
  (cond [(and (not (zero? W)) (empty? lst)) #f]
         [(zero? W) #t]
         [(< W (apply min lst)) #f]
         [(> W (apply + lst)) #f]
         [else (or (can (- W (first lst)) (rest lst))
                   (can W (rest lst)))]))

#;(can 12 '(6 1 6))


(define (3_can W lst)
  (cond  [(andmap zero? W) #t]
         [(empty? lst) #f]
         ;[(ormap negative? W) #f]
         [(> (apply + W) (apply + lst)) #f]
         [else (or (3_can (list (- (first W) (first lst)) (second W) (third W)) (rest lst))
                   (3_can (list (first W) (- (second W) (first lst)) (third W)) (rest lst))
                   (3_can (list (first W) (second W) (- (third W) (first lst))) (rest lst))
                   )]))

(3_can '(36 36 36) '(1 2 3 4 5 5 7 7 8 10 12 19 25)) 
(3_can '(7 7 7) '(6 1 6 1 6 1)) 
(3_can '(7 7 7) '(6 1 6 1 6)) 
(3_can '(4 4 4) '(3 3 3 3)) 

