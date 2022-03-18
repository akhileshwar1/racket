(define (player-acts-on-monsters w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "e" k) (end-turn w)]
    [(key=? "n" k) (initialize-orc-world)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k)  (move-target w -1)]
    [(key=? "down" k)  (move-target w (+ PER-ROW))]
    [(key=? "up" k)    (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)

(define (stab w)
  ((monster-update! set-monster-health! monster-health) (list-ref w-lom w-target) 5))
