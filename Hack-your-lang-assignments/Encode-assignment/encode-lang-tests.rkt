#lang racket

; ---------------------------------------------------------------------------------------------------

(require rackunit)
(require syntax/macro-testing)
(require "encode-lang.rkt")
(require rackunit/text-ui)

; ---------------------------------------------------------------------------------------------------

; Tests

(define encode-tests
  (test-suite
    "All tests"
    (test-begin
      (check-equal? (encode 65 66) "AB")
      (check-equal? (encode 65 66 75) "ABK"))

    ; Compile-time errors.
    (test-begin
      (check-exn #rx"expected exact-nonnegative-integer"
                (lambda () (convert-compile-time-error (encode 65 "A"))))
      (check-exn #rx"expected exact-nonnegative-integer"
                 (lambda () (convert-compile-time-error (encode 65 66 "C"))))
      (check-exn #rx"integer out of bounds" (lambda () (convert-compile-time-error (encode 63 122))))
      (check-exn #rx"integer out of bounds" 
                 (lambda () (convert-compile-time-error (encode 63 129)))))))

(run-tests encode-tests)

