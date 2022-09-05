#lang racket


; ------------------------------------------------------------------------------

(require (for-syntax racket/list))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/stxparam)
(require (for-syntax syntax/parse))

; ------------------------------------------------------------------------------

; Grammar:
;         (define-data-type type-name type-name-predicate
;             {(variant-name {(field-name predicate)}* )}+)
;
;         (cases type-name expression
;            { (variant-name ( { ﬁeld-name } ∗ ) consequent) } ∗
;                (else default))  


; ------------------------------------------------------------------------------

(define-syntax (define-data-type stx)
  (syntax-parse stx
    [(_ type-name predicate (variant-name (field-name field-predicate) ...) 
                                                             ...+)
      #`(begin
        (define variant-name
          (and field-predicate ...)))]))
             
(define-data-type lc-exp lc-exp?
         (var-exp (var symbol?))
         (lambda-exp (var symbol?) (body number?)))

(lc-exp)
