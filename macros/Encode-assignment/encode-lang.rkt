#lang racket

; --------------------------------------------------------------------------------------------------- 

(provide
  ; An encode lang whose top level only consists of integers, and any encode program returns the 
  ; corresponding string
  encode)


; --------------------------------------------------------------------------------------------------- 
(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)
(require (for-syntax syntax/parse))
(require (for-syntax "correct-ints-sc.rkt"))
(require rackunit)
(require syntax/macro-testing)
(require rackunit/text-ui)	

; --------------------------------------------------------------------------------------------------- 

; Implementation

; Interface: (encode 1 2 3 74 ...)
; example: (encode 65 66) => "A B"
(define-syntax (encode stx)
  (syntax-parse stx
    [(_ n ...+)
     #:with (~var ns correct-ints) #'(n ...)
     #`(begin
         #,(list->string (for/list ([int (syntax->datum #'(n ...))])
              (integer->char int))))]))


