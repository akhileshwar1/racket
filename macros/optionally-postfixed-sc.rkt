#lang racket

; ---------------------------------------------------------------------------------------------------

(provide 
  ; syntax class to match identifiers that come with initializers
  optionally-postfixed)

; ---------------------------------------------------------------------------------------------------
(require syntax/parse)
(require "distinct-ids-sc.rkt")

; ---------------------------------------------------------------------------------------------------
(define-syntax-class (optionally-postfixed default)
                     (pattern ((~var x+optional (id-or-id+post default)) ...)
                              #:with (~var ids* distinct-ids) #'(x+optional.id ...)
                              #:with (~var post*) #'(x+optional.post ...)))

(define-syntax-class (id-or-id+post default)
                     (pattern (~var x id)
                              #:attr id #'x
                              #:attr post default)
                     (pattern ((~var x id) (~var p str))
                              #:attr id #'x
                              #:attr post #'p))
