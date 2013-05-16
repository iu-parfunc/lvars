#lang racket

(require redex/reduction-semantics
         "LVish-natpair-ivars.rkt")
(require srfi/1)
(require "../test-helpers.rkt")

(provide
 test-all)

(define (test-all)
  (display "Running metafunction tests...")
  (flush-output)
  (time (meta-test-suite))

  (display "Running test suite...")
  (flush-output)
  (time (program-test-suite rr)))

;; Test suite
(define (meta-test-suite)

  (test-equal
   (term (incomp ((3 Bot) (Bot 4))))
   (term #f))

  (test-equal
   (term (incomp ((2 Bot) (3 Bot) (Bot 4))))
   (term #f))

  (test-equal
   (term (incomp (Bot (4 Bot))))
   (term #f))

  (test-equal
   (term (incomp ((3 Bot) (4 Bot))))
   (term #t))

  (test-equal
   (term (incomp ((Bot 3) (Bot 4))))
   (term #t))

  (test-equal
   (term (incomp ((Bot 1) (Bot 2) (Bot 3) (Bot 4) (Bot 5))))
   (term #t))

  (test-equal
   (term (incomp ((Bot 1) (Bot 2) (Bot 3) (Bot 4) (Bot 5) (1 Bot))))
   (term #f))

  ;; FIXME: write more tests

  (test-results))

(define (program-test-suite rr)

  ;; FIXME: write programs

  (test-results))

