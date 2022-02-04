#lang racket
; Definition for singly-linked list:

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
#;(define (make-list-node [val 0])
  (list-node val #f))


(define/contract (add-two-numbers l1 l2)
  (-> (or/c list-node? #f) (or/c list-node? #f) (or/c list-node? #f))

  (define (add l1 l2 carry)
    (define val1 (list-node-val l1))
    (define val2 (list-node-val l2))
    (define-values (total carry1) (total-carry val1 val2 carry))
    (cond [(and (equal? (list-node-next l1) #f) (equal? (list-node-next l2) #f)) (if (equal? carry 0)
                                                                                     (list-node total #f)
                                                                                     (list-node total (list-node carry #f)))] 
          [else (list-node total (add (rest-l l1) (rest-l l2) carry1 ))]))

  (define (rest-l l1)
    (cond [(list-node-next l1) (list-node-next l1)]
          [else (list-node 0 #f)]))
   
    (define (digits n)
        (if (zero? n)
            '()
            (cons (remainder n 10) (digits (quotient n 10)))))

  (define (total-carry val1 val2 carry)
    (define value (+ val1 val2 carry))
    (define dgs (digits value))
    (cond [(<= value 9) (values value 0)]
          [(> value 9) (values (first dgs) (second dgs))]))
          
  (add l1 l2 0)
  )

(define (generate-linked-nodes lst)
  (cond [(empty? (rest lst)) (list-node (first lst) #f)]
        [else (list-node (first lst) (generate-linked-nodes (rest lst)))]))

(define l1 (generate-linked-nodes '(9 9 9 9 9 9 9)))
(define l2 (generate-linked-nodes '(9 9 9 9)))
(display (add-two-numbers l1 l2))
