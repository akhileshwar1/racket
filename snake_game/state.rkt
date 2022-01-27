#lang racket
(provide (all-defined-out))

(struct posn (x y))
(struct snake (dir segs))
(struct goo (loc expire))
(struct pit (snake goos))
