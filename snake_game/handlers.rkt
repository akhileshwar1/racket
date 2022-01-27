#lang racket
(require "state.rkt")
(require 2htdp/universe 2htdp/image)
(provide (all-defined-out))
;; Constants

;; Tick Rate 
(define TICK-RATE 1/10)

;; Board Size Constants
(define SIZE 30)

;; Snake Constants
(define SEG-SIZE 15)

;; Goo Constants
(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

;; GRAPHICAL BOARD
(define WIDTH-PX  (* SEG-SIZE SIZE))
(define HEIGHT-PX (* SEG-SIZE SIZE))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)

;;GOO functions.
(define (age-goo goos)
  (rot (renew goos)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))
(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

#;(define (decay g)
  (goo (goo-loc g) ((goo-expire g) - 1)))
(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos)) (cons (fresh-goo) (renew (rest goos)))]
        [else (cons (first goos) (renew (rest goos)))]))

(define (rotten? goo)
  (zero? (goo-expire goo)))

#;(define (can-eat snake goos)
  (cond [(empty? goos) empty]
        [else (or (close? snake-head (first goos)) (can-eat snake (rest goos)))]))

;; ON-key functions.
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))


(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (slither sn)
  (snake (snake-dir sn) (cons (next-head sn) (all-but-last (snake-segs sn)))))

#;(define (all-but-last segs)
  (if (rest segs)
      (cons (first segs) (all-but-last (rest segs)))
      (empty)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))
(define (next-head sn)
  (define head (snake-head sn))
  (define dir  (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)])) 

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn)))) 

(define (dir? x)
  (or (key=? x "up")
      (key=? x "down")
      (key=? x "left")
      (key=? x "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))



; RENDER FUNCTIONS
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake) 
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

;; [Listof Goo] Scene -> Scene
;; draws all of the goo to a scene
;; > (goo-list+scene (list goo0) MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (goo-list+scene goos scene)
  ;; [Listof Goo] -> [Listof Posn]
  ;; gets the posns of all the goo 
  ;; > (get-posns-from-goo (list (goo (posn 2 2) 1) (goo (posn 3 3) 1))
  ;; (list (posn 2 2) (posn 3 3))
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))

;; [Listof Posn] Image Scene -> Scene
;; Draws a the image to each posn in the list
;; > (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 8 8
;;              (img-list+scene empty GOO-IMG MT-SCENE))
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))

;; Posn Image Scene -> Scene
;; Draws a the given image onto the scene at the posn.
;; > (img+scene (posn 2 2) GOO-IMG MT-SCENE)
;; (place-image GOO-IMG 32 32 MT-SCENE)
(define (img+scene posn img scene)
  (place-image img 
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))



;; End Functions.
(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

;; Snake -> Boolean
;; Determine if the snake is colliding with any of the walls.
;; > (wall-colliding? (snake "up" (list (posn 0 1))))
;; #t
(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

;; auxiliary functions
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; Snake -> [Listof Segs]
;; returns the snake's body.
;; That is everyting that isn't the snake's head.
(define (snake-body sn)
  (rest (snake-segs sn)))

;; Snake Direction -> Snake 
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
