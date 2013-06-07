#lang racket

(module language racket
  (require "LVish.rkt")
  (provide LVish-natpair-ivars)

  (define-LVish-language LVish-natpair-ivars
    my-lub
    (natural natural)
    (natural Bot)
    (Bot natural))

  ;; my-lub: A function that takes two pairs (they might be of the
  ;; form (natural natural), (natural Bot), or (Bot natural)) and
  ;; returns a pair that is their least upper bound.

  ;; Because they're IVars, we can only safely combine two pairs if one
  ;; of them has only the car filled in, and the other has only the cadr
  ;; filled in.

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

    (test-equal
     (term (lookup-frozenness ((l ((2 3) #f))) l))
     (term #f))

    (test-equal
     (term (lookup-frozenness ((l ((2 3) #t))) l))
     (term #t))

    ;; FIXME: write more tests

    (test-results))

  (define (program-test-suite rr)

    ;; FIXME: write more programs
    
    ;; E-Freeze
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 (3 4))))
                    (freeze x_1 after ())))))
              (term
               (((l ((3 4) #t)))
                (3 4))))

    (test-results)))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))

;; -------------------------------------------------------------------

;; Some tests that *should* work...

;; TODO: don't use `with`.

(module experimental-tests racket
  (require redex/reduction-semantics)
  (require (submod ".." language))

  ;; Should be quasi-deterministic.
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let par
                    ((x_2 (freeze x_1 after () with (put x_1 ((3 Bot)))))
                     (x_3 (put x_1 ((Bot 6)))))
                  x_2))))
            (term
             (((l ((3 6) #t)))
              ((3 6))))
            (term
             Error))

  ;; Should deterministically raise an error, since it never uses
  ;; freezing.
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let par
                    ((x_2 (let ((x_4 (put x_1 ((3 4)))))
                            ;; legal, incompatible 2-element
                            ;; threshold set
                            (get x_4 ((3 4) (6 6)))))
                     (x_3 (put x_1 ((6 6)))))
                  x_2))))
            (term
             Error))

  ;; Should get stuck reducing, since ((3 Bot) (Bot 6)) is an illegal
  ;; threshold set.
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let par
                    ((x_2 (get x_1 ((3 Bot) (Bot 6))))
                     (x_3 (put x_1 ((6 6)))))
                  x_2))))
            (term
             (((l ((6 6) #f)))
              (((lambda (x_2)
                  (lambda (x_3) x_2))
                (get l ((3 Bot) (Bot 6))))
               ())))))


