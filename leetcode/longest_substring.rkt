#lang racket

;; get the length of longest substr of a sting.
;; string -> number
;; (longest-substring "abcabb") => 3
;; (longest-substirng "bbbbb") => 1
(define (longest-substring str)
  (define lst (string->list str))
  )

(longest-substring "abcffg")
