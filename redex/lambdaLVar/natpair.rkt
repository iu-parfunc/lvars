#lang racket

(module language racket
  (require "lambdaLVar.rkt")

  (define-lambdaLVar-language lambdaLVar-natpair
    my-lub
    (natural natural)
    (natural Bot)
    (Bot natural))

  (define my-lub
    (lambda (p1 p2)
      (let* ([x_1 (car p1)]
             [x_2 (cadr p1)]
             [y_1 (car p2)]
             [y_2 (cadr p2)]
             [max_1 (my-max x_1 y_1)]
             [max_2 (my-max x_2 y_2)])
        (list max_1 max_2))))

  (define my-max 
    (lambda (z_1 z_2)
      (cond 
        [(and (number? z_1) (number? z_2))
         (max z_1 z_2)]
        [(number? z_1)
         z_1]
        [(number? z_2)
         z_2]
        [else (error "You didn't pass in a valid argument")]))))

(module test-suite racket
  (require redex/reduction-semantics)
  (require (submod ".." language))
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
     (term (lub Top (3 3)))
     (term Top))

    (test-equal
     (term (lub Bot (3 3)))
     (term (3 3)))

    (test-equal
     (term (lub Bot Bot))
     (term Bot))

    (test-equal
     (term (lub Bot Top))
     (term Top))

    (test-equal
     (term (lub (3 3) (4 4)))
     (term (4 4)))

    (test-equal
     (term (lub (3 3) (3 3)))
     (term (3 3)))

    (test-equal
     (term (lub (3 5) (7 3)))
     (term (7 5)))

    (test-equal
     (term (lub (3 0) (2 0)))
     (term (3 0)))

    (test-equal
     (term (lub (3 Bot) (2 0)))
     (term (3 0)))

    (test-equal
     (term (lub (Bot 4) (2 0)))
     (term (2 4)))

    (test-equal
     (term (lub (Bot 0) (2 0)))
     (term (2 0)))

    (test-equal
     (term (lub (2 0) (2 Bot)))
     (term (2 0)))

    ;; FIXME: write more tests

    (test-results))

  (define (program-test-suite rr)

    ;; FIXME: write programs

    (test-results))

  (define (slow-program-test-suite rr)

    ;; FIXME: slow tests go here, if any
    
    (test-results)))

(module test-fast racket
  (require (submod ".." test-suite))
  (test-fast))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))