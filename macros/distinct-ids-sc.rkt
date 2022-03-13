#lang racket

(provide
  ; syntax class that matches a list of identifiers
  ; and checks for duplicates.
  distinct-ids)

; --------------------------------------------------------------------------------------------------- 
(require syntax/parse)

; ---------------------------------------------------------------------------------------------------

; syntax->syntax or #f
; returns duplicate element as the head of a list.
(define (exists-duplicate stx)
  (check-duplicates (syntax-e stx) free-identifier=?))

(define-syntax-class distinct-ids
                     [pattern ((~var x id) ...)
                              #:fail-when (exists-duplicate #'(x ...))
                              "duplicate identifier found"])
