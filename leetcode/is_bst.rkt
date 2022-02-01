#lang racket
(struct tree (value left right) #:transparent)
(define left_2 (tree 3 empty empty))
(define right_2 (tree 4.5 empty empty))
(define left_3 (tree 5 empty empty))
(define right_3 (tree 7 empty empty))
(define left (tree 4 left_2 right_2))
(define right (tree 6 left_3 right_3))
(define bst (tree 5 left right))

(define (bst-node? tree side r-side r-value parent)
  (define value (tree-value tree))
  (cond [(and (string=? side "left") (string=? r-side "left")) (if (<= value parent)
                                                                     #t
                                                                     #f)]
          [(and (string=? side "left") (string=? r-side "right")) (if (and (<= value parent) (>= value r-value))
                                                                     #t
                                                                     #f)]
          [(and (string=? side "right") (string=? r-side "left")) (if (and (>= value parent) (<= value r-value))
                                                                     #t
                                                                     #f)]
          [(and (string=? side "right") (string=? r-side "right")) (if (>= value parent)
                                                                     #t
                                                                     #f)]

          [else #t]))


(define (is-bst? tree side r-side r-value parent)
  (define value (tree-value tree))
  (define left (tree-left tree))
  (define right (tree-right tree))
  (cond [(and (empty? left) (empty? right)) (bst-node? tree side r-side r-value parent)]
        [else (and (bst-node? tree side r-side r-value parent)
                   (is-bst? left "left"  side parent value)
                   (is-bst? right "right" side parent value))]))

(is-bst? bst "root" "root" 0 0)
