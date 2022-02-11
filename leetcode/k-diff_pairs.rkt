#lang racket

;purpose: get the k-diff unique pairs for a list of numbers and k.
;contract: (list of numbers) (k) -> number of unique pairs.
#| examples:
[3,1,4,1,5], k = 2 -> 2
[1,2,3,4,5], k = 1 -> 4
[1,3,1,5,4], k = 0 -> 1
|#

(define (k-diff lst k)
  (define (list->hash lst)
    (make-hash (map (lambda (x) (cons x x)) lst)))

  (define (diff lst)
    (cond [(empty? lst) 0]
          [else (+ (count (first lst) (list->hash (rest lst))) (diff (rest lst)))]))

  (define (count item hash)
    (define small (- item k))
    (define big (+ item k))
    (cond [(and (not (equal? small big)) (hash-has-key? hash small) (hash-has-key? hash big)) 2]
          [(or (hash-has-key? hash small) (hash-has-key? hash big)) 1]
          [else 0]))
  (diff lst))

(k-diff '(3 1 4 1 5) 2)
(k-diff '(1 2 3 4 5) 1)
(k-diff '(1 3 1 5 4) 0)
