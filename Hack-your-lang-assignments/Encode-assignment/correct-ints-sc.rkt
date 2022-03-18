#lang racket

; --------------------------------------------------------------------------------------------------- 

(provide
  ; syntax-class to check whether all the integers passed are within the range.
  correct-ints)


; --------------------------------------------------------------------------------------------------- 
(require syntax/parse)


; --------------------------------------------------------------------------------------------------- 

; Implementation

; number, number -> boolean
; checking if the number lies in the range.
(define (in-range? n l h)
  (or (< n l) (> n h)))


; syntax->boolean
; checks whether any number is out of range, and immediately breaks.
(define (check-invalid-int stx)
  (define result #f)
  (for ([n (syntax->datum stx)]
        #:when (in-range? n 64 128))
        (set! result #t)
         #:break #t
         (void))
  result)

(define-syntax-class correct-ints
                     [pattern ((~var n nat) ...)
                              #:fail-when (check-invalid-int #'(n ...))
                                          "integer out of bounds"])
                             

