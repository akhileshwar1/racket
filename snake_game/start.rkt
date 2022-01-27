#lang racket
(require "handlers.rkt" "state.rkt")
(require 2htdp/universe 2htdp/image)


(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; Pit -> Pit
;; Take one step: eat or slither
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos  (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

;; Pit KeyEvent -> Pit
;; Handle a key event
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

;; Pit -> Scene
;; Render the world as a scene
(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

;; Pit -> Boolean
;; Is the snake dead?
(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

;; Pit -> Scene
;; produces a gameover image
(define (render-end w)
  (overlay (text "Game over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))
