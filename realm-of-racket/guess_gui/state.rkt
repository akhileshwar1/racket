#lang racket
(provide (struct-out interval))
(struct interval (small big) #:transparent)
