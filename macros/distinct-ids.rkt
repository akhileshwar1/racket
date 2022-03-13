#lang racket

;; =================================================================================================== 
(require loop)
(require (for-syntax racket/match))
(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require macro-debugger/stepper)
(require macro-debugger/expand)
(require macro-debugger/stepper-text)
(require (for-syntax syntax/parse))

;; SO basically the problem was that you wanted to match a syntax variable with a list of identifiers.
;; but you did not use (x ...) because in this form it can only be used in a template.
;; And we want to use it as a variable in the pattern itself, to constraint it.
;; THerefore we use dot notation to have it a meaning of list in the pattern itself.
;; other wise it is interpreted as a seq of terms only in the template. 

(define-syntax (test stx)
  (syntax-parse stx
    [(_ firs . (~var res)) 
     (displayln #'res)
     #`(apply + res)]))

(define-syntax (testt stx)
  (syntax-parse stx
    [(_ firs  (~var res) ...) 
     (displayln #'(res ...))
     #'(displayln firs)]))

;; The clear difference is that with (res ...) matches a list, and .res matches with list.
(test 1 2 3 4)
(testt 1 2 3 4)
