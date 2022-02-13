#lang racket

;purpose: get the k-diff unique pairs for a list of numbers and k.
;contract: (list of numbers) (k) -> number of unique pairs.
#| examples:
[3,1,4,1,5], k = 2 -> 2
[1,2,3,4,5], k = 1 -> 4
[1,3,1,5,4], k = 0 -> 1
|#

(define (find-pairs lt k)
  (define (list->hash lst)
    (make-hash (map (lambda (x) (cons x x)) lst)))


  (define (main)
      (define lst (list->set lt))
      ;; hash table from the list.
      (define hash (list->hash lt))
      (cond [(zero? k) (count-elements-having-duplicates lt)]
            [else (unlist-and-filter-unique-pairs (for/list ([item lst])
                (get-pairs item hash)))]))

  ;; purpose: get all the pairs related to an item.
  ;; contract: number hash-table -> list of pairs.
  (define (get-pairs item hash)
    (define small (- item k))
    (define big (+ item k))
    (cond [(and (not (equal? small big)) (hash-has-key? hash small) (hash-has-key? hash big)) (list (cons item small) (cons item big))]
          [(hash-has-key? hash small) (list (cons item small))]
          [(hash-has-key? hash big) (list (cons item big))]
          [else '()]))
    
    ;; purpose: convert a list of list of pairs to a list of pairs.
    ;; contract: list of list of pairs -> list of pairs.
    (define (unlist lst)
      (foldr (lambda (lst rest)
                     (cond [(empty? lst) rest]
                           [(empty? (cdr lst)) (cons (car lst) rest)]
                           [else (cons (car lst) (cons (cadr lst) rest))])) '() lst))

    ;; purpose: filter the unique pairs from a list of pairs using the logic that two pairs would be the same if they include the same max number.
    ;; contract: list of pairs -> count unique pairs.
    (define (filter-unique-pairs lst)
       (hash-count (make-hash (map (lambda (pair) (define value (max (car pair) (cdr pair)))
                                     (cons value value)) lst))))

    ;; purpose: self explanatory.
    ;; contract: list -> list.
    (define (unlist-and-filter-unique-pairs lst)
      (filter-unique-pairs (unlist lst)))

    ;; purpose: remove the duplicate elements from a list.
    ;; contract: list -> unique list.
    (define (list->set lst)
      (foldr (lambda (item rest)
                     (if (member item rest)
                          rest
                         (cons item rest))) empty lst))
   
    ;; purpose: used in case k = 0. Returns a list computed from a hash table of elements and their frequencies.
    ;; contract: list -> list of numbers having duplicates.
    (define (elements-having-duplicates lst)
      (hash-map (foldr (lambda (key rest-hash)
                     (if (hash-has-key? rest-hash key)
                         (hash-update rest-hash key add1)
                         (hash-update rest-hash key add1 0))) (hash) lst) (lambda (key value)
                                                                             (if (> value 1)
                                                                                  key
                                                                                  (void)))))
    ;; purpose: self explanatory.
    ;; contract: list -> number.
    (define (count-elements-having-duplicates lst)
      (length (filter number? (elements-having-duplicates lst))))
      
       

    ;; START HERE
    ;; (count-unique-duplicates '(1 2 2 2 3 3 4))
    (main))

#|(unlist (list  '() (list (cons 1 3)) (list (cons 5 6) (cons 3 3))))
(filter-unique-pairs (unlist (list (list (cons 1 3) (cons 3 1) (cons 3 2)) (list (cons 4 2) (cons 2 4) (cons 5 3)))))
|#
(find-pairs '(3 1 4 1 5) 2)
(find-pairs '(1 2 3 4 5) 1)
(find-pairs '(1 3 1 5 4) 0)
