#lang racket

(require redex/reduction-semantics
         "natpair-ivars.rkt")
(require srfi/1)
(require "../test-helpers.rkt")

(provide
 test-fast
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

;; Warning: Passing `slow-rr` to this procedure will take
;; several orders of magnitude longer to finish than passing
;; `fast-rr`.
(define (slow-program-test-suite rr)

  ;; FIXME: slow tests go here, if any
  
  (test-results))
