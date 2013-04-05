#lang racket
(require srfi/1)
(require "nat/test.rkt")

(provide test-fast
         test-all)

(define (test-fast)
  (display "Running metafunction tests...")
  (flush-output)
  (time (meta-test-suite))

  (display "Running test suite with fast-rr...")
  (flush-output)
  (time (program-test-suite fast-rr))

  (display "Running test suite with slow-rr...")
  (flush-output)
  (time (program-test-suite slow-rr))

  (display "Running slow test suite with fast-rr...")
  (flush-output)
  (time (slow-program-test-suite fast-rr)))

(define (test-all)
  (test-fast)
  (display "Running slow test suite with slow-rr...")
  (flush-output)
  (time (slow-program-test-suite slow-rr)))

