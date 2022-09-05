#lang eopl

(require "let_lang.rkt")
;(require "expval_dtype.rkt")
; ------------------------------------------------------------------------------
;(display (init-env))

(define program "let a = 100 in 
                      let b =  200 in 
                          -(b,a)")

(define proc-program "let a = 100 in 
                          let f = proc (y z) -(a,-(y,z)) in
                            (f 5 4)")


(display (run program))
(display (run proc-program))


