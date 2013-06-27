#lang racket

(module language racket
  (require "LVish.rkt")
  (require srfi/1)
  (require racket/match)
  (provide LVish-natpair-ivars)
  (provide downset-op)

  (define-LVish-language LVish-natpair-ivars
    downset-op
    my-lub
    (natural natural)
    (natural Bot)
    (Bot natural))

  ;; downset-op: Takes a pair p and returns a list of everything below
  ;; or equal to p in the lattice.  (Note that we're dealing with
  ;; pairs of IVars, not pairs of counters.)

  ;; (downset-op '(2 1)) =>
  ;; '(Bot
  ;;   (2 Bot)
  ;;   (Bot 1)
  ;;   (2 1)) 

  (define downset-op
    (lambda (p)
      (append '(Bot)
              (match p
                ['Bot '()]
                [`(,(? number? a) Bot)
                 `(,p)]
                [`(Bot ,(? number? d))
                 `(,p)]
                [`(,(? number? a) ,(? number? d)) 
                 `((,a Bot)
                   (Bot ,d)
                   ,p)]))))

  ;; my-lub: A function that takes two pairs (they might be of the
  ;; form (natural natural), (natural Bot), or (Bot natural)) and
  ;; returns a pair that is their least upper bound.

  ;; Because they're IVars, we can only safely combine two pairs if
  ;; one of them has only the car filled in, and the other has only
  ;; the cadr filled in -- or, if they've filled in something
  ;; overlapping, if it's equal.

  (define lub-helper
    (lambda (a1 a2)
      (match `(,a1 ,a2)
        [`(Bot Bot) 'Bot]
        [`(Bot ,(? number? n)) n]
        [`(,(? number? n) Bot) n]
        [`(,(? number? n) ,(? number? n)) n]
        [`(,(? number? n) ,(? number? m)) 'Top])))

  (define my-lub
    (lambda (p1 p2)
      (let ([p `(,(lub-helper (car p1) (car p2))
                 ,(lub-helper (cadr p1) (cadr p2)))])
        (match p
          [`(Top ,_) 'Top]
          [`(,_ Top) 'Top]
          [`(Bot Bot) 'Bot]
          [_ p])))))

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
     (downset-op '(2 1))
     '(Bot
       (2 Bot)
       (Bot 1)
       (2 1)))

    (test-equal
     (downset-op 'Bot)
     '(Bot))

    (test-equal
     (downset-op '(2 Bot))
     '(Bot
       (2 Bot)))

    (test-equal
     (downset-op '(Bot 1))
     '(Bot
       (Bot 1)))

    (test-equal
     (term (lub-p ((3 Bot) #f) ((3 6) #f)))
     (term ((3 6) #f)))

    (test-equal
     (term (incomp (((3 Bot) #f) ((Bot 4) #f))))
     (term #f))

    (test-equal
     (term (incomp (((2 Bot) #f) ((3 Bot) #f) ((Bot 4) #f))))
     (term #f))

    (test-equal
     (term (incomp ((Bot #f) ((4 Bot) #f))))
     (term #f))

    (test-equal
     (term (incomp (((3 Bot) #f) ((4 Bot) #f))))
     (term #t))

    (test-equal
     (term (incomp (((Bot 3) #f) ((Bot 4) #f))))
     (term #t))

    (test-equal
     (term (incomp (((Bot 1) #f) ((Bot 2) #f) ((Bot 3) #f) ((Bot 4) #f) ((Bot 5) #f))))
     (term #t))

    (test-equal
     (term (incomp (((Bot 1) #f) ((Bot 2) #f) ((Bot 3) #f) ((Bot 4) #f) ((Bot 5) #f) ((1 Bot) #f))))
     (term #f))

    (test-equal
     (term (incomp (((3 Bot) #t) ((Bot 4) #t))))
     (term #t))

    (test-equal
     (term (incomp (((Bot 1) #t) ((1 Bot) #t))))
     (term #t))

    (test-equal
     (term (lookup-status ((l ((2 3) #f))) l))
     (term #f))

    (test-equal
     (term (lookup-status ((l ((2 3) #t))) l))
     (term #t))

    (test-equal
     (term (extend-Df () (3 3)))
     (term ((3 3))))

    (test-equal
     (term (extend-Df ((3 3) (4 4) (5 5)) (6 6)))
     (term ((6 6) (3 3) (4 4) (5 5))))

    (test-equal
     (term (contains-all-leq (1 1) (Bot (Bot 1) (1 Bot) (1 1))))
     (term #t))

    (test-equal
     (term (contains-all-leq (1 1) ((Bot 1) (1 Bot) (1 1))))
     (term #f))

    (test-equal
     (term (contains-all-leq (1 1) (Bot (Bot 1) (1 Bot) (2 Bot) (2 0) (2 1))))
     (term #f))

    ;; For the next few tests, note that (downset (1 1)) =>
    ;; '(Bot
    ;;   (1 Bot)
    ;;   (Bot 1)
    ;;   (1 1))

    (test-equal
     (term (first-unhandled-d (1 1) ((Bot 0) (Bot 1))))
     (term Bot))
    
    (test-equal
     (term (first-unhandled-d (1 1) (Bot (0 Bot) (1 Bot))))
     (term (Bot 1)))

    (test-equal
     (term (first-unhandled-d (1 1)
                              (Bot (Bot 1) (1 Bot) (1 1) (5 5) (6 6) (7 7))))
     (term #f))

    (test-equal
     (term (first-unhandled-d (1 1)
                              (Bot (Bot 1) (1 Bot) (1 0) (1 1))))
     (term #f))

    (test-equal
     (term (first-unhandled-d (1 1)
                              (Bot (1 Bot) (1 0) (1 1))))
     (term (Bot 1)))

    (test-equal
     (term (first-unhandled-d (1 1)
                              (Bot (Bot 0) (Bot 1)
                               (0 Bot) (0 0) (0 1)
                               (1 Bot) (1 0))))

     (term (1 1)))

    (test-equal
     (term (first-unhandled-d (1 1)
                              (Bot (Bot 0) (Bot 1)
                               (0 Bot) (0 0) (0 1)
                               (1 Bot) (1 0)
                               (5 5) (6 6) (7 7))))
     (term (1 1)))

    (test-equal
     (term (first-unhandled-d (1 1)
                              ((1 Bot) (0 0) (7 7)
                               (5 5) Bot (1 0)
                               (Bot 0) (1 1) (Bot 1) 
                               (0 Bot) (6 6) (0 1))))
     (term #f))

    (test-results))

  (define (program-test-suite rr)

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

    ;; Quasi-determinism with freezing.
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (let ((x_4 (put x_1 (3 Bot))))
                              (freeze x_1 after ())))
                       (x_3 (put x_1 (Bot 6))))
                    x_2))))
              (term
               (((l ((3 6) #t)))
                (3 6)))
              (term
               Error))

    ;; Should deterministically raise an error, since it never uses
    ;; freezing.
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (let ((x_4 (put x_1 (3 4))))
                              ;; legal, incompatible 2-element
                              ;; threshold set
                              (get x_4 ((3 4) (6 6)))))
                       (x_3 (put x_1 (6 6))))
                    x_2))))
              (term
               Error))

    ;; Trying out more interesting eval contexts.
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 new))
                    (let par
                        ((x_3 (freeze x_1 after ((lambda (x)
                                                  (lambda (x)
                                                    (put x_2 (Bot 0))))
                                                 ())))
                         (x_4 (freeze x_2 after ((lambda (x)
                                                   (lambda (x)
                                                     (put x_1 (0 Bot))))
                                                 ()))))
                      x_3)))))
              (term
               (((l ((0 Bot) #t))
                 (l1 ((Bot 0) #t)))
                (0 Bot)))
              (term
               Error))

    ;; Should get stuck reducing, since ((3 Bot) (Bot 6)) is an
    ;; illegal threshold set.  (Actually, this isn't quite right; such
    ;; programs should be ruled out from the start somehow.)
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (get x_1 ((3 Bot) (Bot 6))))
                       (x_3 (put x_1 (6 6))))
                    x_2))))

              ;; FIXME: Is there a way to just specify "gets stuck
              ;; reducing"?  This overspecifies; I don't really care
              ;; *how* it gets stuck.
              (term
               (((l ((6 6) #f)))
                (((lambda (x_2)
                    (lambda (x_3) x_2))
                  (get l ((3 Bot) (Bot 6))))
                 ()))))

    (test-results)))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))


