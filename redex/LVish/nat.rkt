#lang racket

(module language racket
  (require "LVish.rkt")
  (require srfi/1)
  (provide LVish-nat)
  (provide downset-op) ;; temp
  
  (define-LVish-language LVish-nat downset-op max natural)

  ;; To figure out at some point: maybe we could actually write
  ;; downset-op with Redex patterns?

  ;; (define downset-op
  ;;   (lambda (d)
  ;;     (term (side-condition natural (<= natural d)))))

  (define downset-op
    (lambda (d)
      (append (iota d) `(,d)))))

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
     (term (exists-d 6 ()))
     (term #f))

    (test-equal
     (term (exists-d 6 (3)))
     (term 3))

    (test-equal
     (term (exists-d 6 (9)))
     (term #f))

    (test-equal
     (term (exists-d 3 (3)))
     (term 3))

    ;; These next three are unrealistic for this lattice because Q would
    ;; be a singleton set, but it's here to exercise exists-d.
    (test-equal
     (term (exists-d 6 (7 8 9)))
     (term #f))

    (test-equal
     (term (exists-d 6 (7 8 9 6)))
     (term 6))

    (test-equal
     (term (exists-d 6 (7 8 9 5)))
     (term 5))

    (test-equal
     (term (lub Bot Bot))
     (term Bot))

    (test-equal
     (term (lub Top 3))
     (term Top))

    (test-equal
     (term (lub 3 4))
     (term 4))

    (test-equal
     (term (lub 3 3))
     (term 3))

    (test-equal
     (term (leq 3 3))
     (term #t))

    (test-equal
     (term (leq Top 3))
     (term #f))

    (test-equal
     (term (leq 3 Top))
     (term #t))
    
    (test-equal
     (term (leq Bot 3))
     (term #t))

    (test-equal
     (term (leq 3 Bot))
     (term #f))

    (test-equal
     (term (leq Top Bot))
     (term #f))

    (test-equal
     (term (leq Bot Top))
     (term #t))

    (test-equal
     (term (leq 3 4))
     (term #t))

    (test-equal
     (term (leq 4 3))
     (term #f))

    (test-equal
     (term (extend-Df () 3))
     (term (3)))

    (test-equal
     (term (extend-Df (3 4 5) 6))
     (term (6 3 4 5)))

    (test-equal
     (term (contains-all-leq 3 (0 1 2 3)))
     (term #t))

    (test-equal
     (term (contains-all-leq 3 (1 2 3)))
     (term #f))

    (test-equal
     (term (contains-all-leq 3 (2 3)))
     (term #f))

    (test-equal
     (term (contains-all-leq 3 (2 3 4 5)))
     (term #f))

    (test-equal
     (term (contains-all-leq 3 (0 1 2 3 4 5)))
     (term #t))

    ;; For the next few tests, note that (downset 3) => (0 1 2 3)
    (test-equal
     (term (first-unhandled-d 3 (1 2 3 4 5)))
     (term 0))

    (test-equal
     (term (first-unhandled-d 3 (0 1 2 3 4 5)))
     (term #f))

    (test-equal
     (term (first-unhandled-d 3 (0 1 2 3)))
     (term #f))

    (test-equal
     (term (first-unhandled-d 3 (2 3)))
     (term 0))

    (test-equal
     (term (first-unhandled-d 3 (0 2 3)))
     (term 1))

    (test-equal
     (term (first-unhandled-d 3 (0 1 2)))
     (term 3))

    (test-equal
     (term (first-unhandled-d 3 (0 1 2 4 5 6 7)))
     (term 3))

    (test-equal
     (term (first-unhandled-d 3 (7 0 2 6 3 1 5 4)))
     (term #f))

    (test-equal
     (term (first-unhandled-d 3 (7 6 5 4 3 0)))
     (term 1))

    (test-equal
     (term (store-dom ((l1 (4 #f)) (l2 (5 #f)) (l3 (Bot #f)))))
     (term (l1 l2 l3)))
    
    (test-equal
     (term (lookup-val ((l (2 #f))) l))
     (term 2))

    (test-equal
     (term (lookup-frozenness ((l (2 #f))) l))
     (term #f))

    (test-equal
     (term (lookup-frozenness ((l (2 #t))) l))
     (term #t))
    
    (test-equal
     (term (update-val () l 4))
     (term ((l (4 #f)))))
    
    (test-equal
     (term (update-val ((l (3 #f))) l 4))
     (term ((l (4 #f)))))

    (test-equal
     (term (update-val () l Bot))
     (term ((l (Bot #f)))))

    (test-equal
     (term (store-dom ()))
     (term ()))

    (test-equal
     (term (store-dom ((l (3 #f)) (l1 (4 #f)))))
     (term (l l1)))

    (test-equal
     (term (store-dom-diff ((l (3 #f)) (l1 (4 #f)))
                           ((l (4 #f)) (l1 (3 #f)))))
     (term ()))

    (test-equal
     (term (store-dom-diff ((l (3 #f)))
                           ((l (4 #f)) (l1 (3 #f)))))
     (term ()))

    (test-equal
     (term (store-dom-diff ((l (4 #f)) (l1 (3 #f)))
                           ((l (3 #f)))))
     (term (l1)))

    (test-equal
     (term (store-dom-diff ((l (4 #f)))
                           ()))
     (term (l)))

    (test-equal
     (term (store-top? ()))
     (term #f))

    (test-equal
     (term (store-top? ((l (3 #f)) (l1 (4 #f)))))
     (term #f))

    (test-equal
     (term (store-top? TopS))
     (term #t))

    (test-equal
     (term (top? Top))
     (term #t))

    (test-equal
     (term (top? Bot))
     (term #f))

    (test-equal
     (term (top? 3))
     (term #f))

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l (4 #f)) (l1 (3 #f))) ())
      '(((l1 (3 #f)) (l (4 #f))) ()))
     #t)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l1 (3 #f)) (l (4 #f))) ())
      '(((l1 (3 #f)) (l (4 #f))) (3)))
     #f)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l (4 #f)) (l1 (3 #f))) ())
      '(((l1 (3 #f)) (l (4 #f))) (3)))
     #f)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l (3 #f)) (l1 (4 #f))) ())
      '(((l1 (3 #f)) (l (4 #f))) ()))
     #f)

    (test-equal
     (term (subst l l1 (((l (Bot #f)))
                        (put l 3))))
     (term (((l1 (Bot #f)))
            (put l1 3))))

    (test-results))

  (define (program-test-suite rr)

    ;; E-App-1
    (test-->> rr
              (term
               (() ;; empty store
                ((lambda (x_1) x_1)
                 ((lambda (x_1) x_1) (lambda (x_2) x_2)))))
              (term
               (()
                (lambda (x_2) x_2))))

    ;; E-App-2
    (test-->> rr
              (term
               (() ;; empty store
                (((lambda (x_1) x_1) (lambda (x_2) x_2))
                 (lambda (x_1) x_1))))
              (term
               (()
                (lambda (x_1) x_1))))

    ;; E-ParApp
    (test-->> rr
              (term
               (() ;; empty store
                (((lambda (x_1) x_1) (lambda (x_2) x_2))
                 ((lambda (x_1) x_1) (lambda (x_2) x_2)))))
              (term
               (()
                (lambda (x_2) x_2))))

    ;; E-Beta
    (test-->> rr
              (term
               (() ;; empty store
                ((lambda (x_1) x_1) ())))
              (term
               (()
                ())))

    (test-->> rr
              (term
               (() ;; empty store
                ((lambda (x_1) x_1) (lambda (x_2) x_2))))
              (term
               (()
                (lambda (x_2) x_2))))

    ;; E-New
    (test-->> rr
              (term
               (((l (3 #f)))
                new))
              (term
               (((l (3 #f)) (l1 (Bot #f)))
                l1)))
    
    (test-->> rr
              (term
               (((l (3 #f)) (l1 (4 #f)))
                new))
              (term
               (((l (3 #f)) (l1 (4 #f)) (l2 (Bot #f)))
                l2)))

    ;; E-Put
    (test-->> rr
              (term
               (((l (Bot #f)))
                (put l 3)))
              (term
               (((l (3 #f)))
                ())))
    
    (test-->> rr
              (term
               (((l (2 #f)))
                (put l 3)))
              (term
               (((l (3 #f)))
                ())))

    ;; This should work because put just puts the max of the current
    ;; value and the new value.
    (test-->> rr
              (term
               (((l (2 #f)))
                (put l 1)))
              (term
               (((l (2 #f)))
                ())))
    
    ;; let
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 (lambda (x_1) x_1)))
                  (let ((x_2 (lambda (x_1) x_1)))
                    (x_1 x_2)))))
              (term
               (()
                (lambda (x_1) x_1))))

    ;; let par
    (test-->> rr
              (term
               (() ;; empty store
                (let par ((x_1 (lambda (x_1) x_1))
                          (x_2 (lambda (x_1) x_1)))
                  (x_1 x_2))))
              (term
               (()
                (lambda (x_1) x_1))))

    ;; E-Beta + E-New
    (test-->> rr
              (term
               (() ;; empty store
                ((lambda (x) x) new)))
              (term
               (((l (Bot #f)))
                l)))

    ;; let + E-New + E-Put + E-Get
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 3)))
                    (let ((x_3 (get x_1 (2))))
                      x_3)))))
              (term
               (((l (3 #f)))
                2)))
    
    ;; let par + E-New + E-Put + E-Get
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par ((x_2 (put x_1 2))
                            (x_3 (put x_1 3)))
                    (get x_1 (2))))))
              (term
               (((l (3 #f)))
                2)))

    ;; Another aspect of E-Put's behavior
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 5)))
                    ;; This should just take the lub of the old and new
                    ;; values, i.e., 5.
                    (let ((x_3 (put x_1 4)))
                      (get x_1 (5)))))))
              (term
               (((l (5 #f)))
                5)))

    ;; E-Put-Err
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 Top)))
                    x_2))))
              (term
               Error))

    (test-->> rr
              #:equiv cfgs-equal-modulo-perms?
              (term
               (()
                (let par ([x_1 new]
                          [x_2 new])
                  (let par ([x_3 (put x_1 3)]
                            [x_4 (put x_2 4)])
                    (get x_2 (4))))))
              (term
               (((l (3 #f))
                 (l1 (4 #f)))
                4))
              (term
               (((l (4 #f))
                 (l1 (3 #f)))
                4)))
    
    ;;let par + E-New + E-Put + E-Get
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par ((x_2 (put x_1 2))
                            (x_3 (get x_1 (2))))
                    (get x_1 (2))))))
              (term
               (((l (2 #f)))
                2)))

    ;; let par + E-New + E-Put + E-Get
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ;; Gets stuck trying to get 4 out of x_1, then
                      ;; unstuck after the other subexpression finishes.
                      ((x_4 (let par ((x_2 (put x_1 2))
                                      (x_3 (put x_1 3)))
                              (get x_1 (4))))
                       ;; Eventually puts 4 in x_1 after several dummy
                       ;; beta-reductions.
                       (x_5 ((lambda (x_2)
                               ((lambda (x_2)
                                  ((lambda (x_2)
                                     ((lambda (x_2)
                                        ((lambda (x_2)
                                           (put x_1 4)) ())) ())) ())) ())) ())))
                    x_4))))
              (term
               (((l (4 #f)))
                4)))

    ;; E-Freeze
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 3)))
                    (freeze x_1 after ())))))
              (term
               (((l (3 #t)))
                3)))

    ;; Here we have a quasi-deterministic program where a freeze-after
    ;; and a put are racing with each other.  One of two things will
    ;; happen: (put x_1 (4)) will complete first, so x_2 will be (4),
    ;; or the freeze-after will complete first, so the program will
    ;; raise an error.
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (let ((x_4 (put x_1 3)))
                              (freeze x_1 after ())))
                       (x_3 (put x_1 4)))
                    x_2))))
              (term
               (((l (4 #t)))
                4))
              (term
               Error))

    ;; Fancier freezing.  This one will actually never raise an error
    ;; because the racing put is less than 5!
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (let ((x_4 (put x_1 3)))
                              (freeze x_1 after (lambda (x)
                                                  (put x_1 5)))))
                       (x_3 (put x_1 4)))
                    x_2))))
              (term
               (((l (5 #t)))
                5)))
    
    ;; But this one might:
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ((x_2 (let ((x_4 (put x_1 3)))
                              (freeze x_1 after (lambda (x)
                                                  (put x_1 5)))))
                       (x_3 (put x_1 6)))
                    x_2))))
              (term
               (((l (6 #t)))
                6))
              (term
               Error))

    (test-results)))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))
