#lang racket
(require srfi/1)
(require "lambdaLVar-nat-test.rkt")

(provide test-fast
         test-all)

;; Takes two (S e) configurations and returns #t if they're equal
;; modulo permutations of store bindings.
(define cfgs-equal-modulo-perms?
  (lambda (cfg1 cfg2)
    (and (stores-equal-modulo-perms? (car cfg1) (car cfg2))
         (equal? (cdr cfg1) (cdr cfg2)))))

;; Takes two stores and returns #t if they're equal modulo
;; permutations.
(define stores-equal-modulo-perms?
  (lambda (s1 s2)
    (lset= equal? s1 s2)))

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

