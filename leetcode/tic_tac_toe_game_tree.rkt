#lang racket

(struct gt (board trees)#:transparent)
(struct box (pos player)#:transparent)

; purpose: generate the game tree starting from an empty board.
; contract: player board-> game-tree-- which is list of trees.
(define (game-tree player board)
  (define posns (get-free-posns board))
  (for/list ([posn posns])
            (define new-board (make-new-board (box posn player) board)) 
            (gt new-board (game-tree (alternate-player player) new-board))))

; purpose: make a new board given a board and a new box.
; contract: box board -> new board
(define (make-new-board n-box board)
  (for/list ([box board])
            (cond [(equal? (box-pos box) (box-pos n-box)) n-box]
                  [else box])))

; purpose: get the free posns from the board.
; contract: board -> list of free indexes.
(define (get-free-posns board)
  (for/list ([box board]
             #:when (equal? (box-player box) "N"))
            (box-pos box)))

; purpose: get the other player
; contract: player -> player
(define (alternate-player player)
  (if (equal? player "X")
      "0"
      "X"))

;purpose: get the empty board to start with.
(define (generate-empty-board)
  (for/list ([i (in-range 9)])
            (box i "N")))

(define empty-board (generate-empty-board))
(display (game-tree "X" empty-board))
  
