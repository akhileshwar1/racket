(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monsters)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))


(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))


(define (initialize-monsters)
  (build-list
    MONSTER#
    (lambda (_)
      (define health (random+ MONSTER-HEALTH0))
      (case (random 4)
        [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
        [(1) (hydra HYDRA-IMAGE health)]
        [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
        [(3) (brigand BRIGAND-IMAGE health)]))))

