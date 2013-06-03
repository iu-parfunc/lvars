#lang racket

(module language racket
  (require redex/reduction-semantics)
  (require "lambdaLVar.rkt")

  (define-lambdaLVar-language lambdaLVar-natpair-ivars
    my-lub
    (natural natural)
    (natural Bot)
    (Bot natural))

  ;; my-lub: A function that takes two pairs (they might be of the
  ;; form (natural natural), (natural Bot), or (Bot natural)) and
  ;; returns a pair that is their least upper bound.

  ;; Because they're IVars, we can only safely combine two pairs if
  ;; one of them has only the car filled in, and the other has only
  ;; the cadr filled in.

  ;; assumes that a1 and a2 aren't both numbers
  (define lub-helper
    (lambda (a1 a2)
      (cond
        [(and (number? a1) (number? a2))
         ;; If we get here, something's wrong
         (error "oops!")]
        [(number? a1) a1]
        [(number? a2) a2]
        [else 'Bot])))

  (define my-lub
    (lambda (p1 p2)
      (let ([car1 (car p1)]
            [cadr1 (cadr p1)]
            [car2 (car p2)]
            [cadr2 (cadr p2)])
        (cond
          ;; nat/Bot, nat/Bot
          ;; nat/Bot, nat/nat
          ;; nat/nat, nat/Bot
          ;; nat/nat, nat/nat
          [(and (number? car1) (number? car2))
           'Top]

          ;; Bot/nat, Bot/nat
          ;; nat/nat, Bot/nat
          ;; Bot/nat, nat/nat
          [(and (number? cadr1) (number? cadr2))
           'Top]

          ;; nat/Bot, Bot/nat
          ;; Bot/nat, nat/Bot
          [else (list
                 (lub-helper car1 car2)
                 (lub-helper cadr1 cadr2))])))))

(module test-suite racket
  (require redex/reduction-semantics)
  (require (submod ".." language))
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

  (define (slow-program-test-suite rr)

    ;; FIXME: slow tests go here, if any
    
    (test-results)))

(module test-fast racket
  (require (submod ".." test-suite))
  (test-fast))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))


