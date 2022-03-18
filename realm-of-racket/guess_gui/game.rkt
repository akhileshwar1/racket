#lang racket
(require 2htdp/universe 2htdp/image)
(require "state.rkt" "handlers.rkt")

(define (start lower upper)
  (big-bang (interval lower upper)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))



