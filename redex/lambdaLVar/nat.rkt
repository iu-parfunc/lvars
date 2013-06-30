#lang racket

(module language racket
  (require "lambdaLVar.rkt")
  (define-lambdaLVar-language lambdaLVar-nat max natural))

(module test-suite racket
  (require redex/reduction-semantics)
  (require (submod ".." language))
  (require srfi/1)
  (require "test-helpers.rkt")
  
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
     (term (store-dom ((l1 4) (l2 5) (l3 Bot))))
     (term (l1 l2 l3)))

    (test-equal
     (stores-equal-modulo-perms?
      (term (lubstore ((l1 5)
                       (l2 6)
                       (l3 7))
                      ((l2 2)
                       (l4 9))))
      (term ((l1 5)
             (l3 7)
             (l2 6)
             (l4 9))))
     #t)

    (test-equal
     (stores-equal-modulo-perms?
      (term (lubstore ((l1 5)
                       (l2 6)
                       (l3 7))
                      ((l1 5)
                       (l4 9)
                       (l2 4))))
      (term ((l3 7)
             (l1 5)
             (l4 9)
             (l2 6))))
     #t)

    (test-equal
     (stores-equal-modulo-perms?
      (term (lubstore ((l1 Bot)
                       (l2 6)
                       (l3 Bot))
                      ((l1 5)
                       (l4 9)
                       (l2 4))))
      (term ((l3 Bot)
             (l1 5)
             (l4 9)
             (l2 6))))
     #t)

    (test-equal
     (term (lubstore-helper ((l1 5))
                            ()
                            l1))
     (term 5))

    (test-equal
     (term (lubstore-helper ((l1 5))
                            ((l1 6))
                            l1))
     (term 6))

    (test-equal
     (term (lubstore-helper ((l1 5)
                             (l2 6)
                             (l3 7))
                            ((l2 2)
                             (l4 9))
                            l2))
     (term 6))
    
    (test-equal
     (lset= equal?
            (lset-union equal? (term ()) (term ()))
            (term ()))
     #t)

    (test-equal
     (lset= equal?
            (lset-union equal? (term ()) (term (l1)))
            (term (l1)))
     #t)

    (test-equal
     (lset= equal?
            (lset-union equal? (term (l1 l2)) (term (l1 l2 l3)))
            (term (l1 l2 l3)))
     #t)

    (test-equal
     (lset= equal?
            (lset-union equal? (term (l2 l3)) (term (l1 l4)))
            (term (l2 l3 l1 l4)))
     #t)

    (test-equal
     (lset= equal?
            (lset-union equal? (term (l2 l3)) (term (l1 l2 l4)))
            (term (l3 l1 l2 l4)))
     #t)

    (test-equal
     (term (store-lookup ((l 2)) l))
     (term 2))
    
    (test-equal
     (term (store-update () l 4))
     (term ((l 4))))
    
    (test-equal
     (term (store-update ((l 3)) l 4))
     (term ((l 4))))

    (test-equal
     (term (store-update () l Bot))
     (term ((l Bot))))

    (test-equal
     (term (valid ()))
     #f)

    (test-equal
     (term (valid (3)))
     #t)

    (test-equal
     (term (valid (5 6 7)))
     #t)

    (test-equal
     (term (store-dom ()))
     (term ()))

    (test-equal
     (term (store-dom ((l 3) (l1 4))))
     (term (l l1)))

    (test-equal
     (term (store-dom-diff ((l 3) (l1 4))
                           ((l 4) (l1 3))))
     (term ()))

    (test-equal
     (term (store-dom-diff ((l 3))
                           ((l 4) (l1 3))))
     (term ()))

    (test-equal
     (term (store-dom-diff ((l 4) (l1 3))
                           ((l 3))))
     (term (l1)))

    (test-equal
     (term (store-dom-diff ((l 4))
                           ()))
     (term (l)))

    (test-equal
     (term (rename-locs (((l Bot))
                         (put l (3)))
                        ((l 4))
                        ()))
     (term
      (((l1 Bot))
       (put l1 (3)))))

    (test-equal
     (term (store-top? ()))
     (term #f))

    (test-equal
     (term (store-top? ((l 3) (l1 4))))
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
      '(((l 4) (l1 3)) ())
      '(((l1 3) (l 4)) ()))
     #t)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l1 3) (l 4)) ())
      '(((l1 3) (l 4)) (3)))
     #f)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l 4) (l1 3)) ())
      '(((l1 3) (l 4)) (3)))
     #f)

    (test-equal
     (cfgs-equal-modulo-perms?
      '(((l 3) (l1 4)) ())
      '(((l1 3) (l 4)) ()))
     #f)

    (test-equal
     (term (subst l l1 (((l Bot))
                        (put l (3)))))
     (term (((l1 Bot))
            (put l1 (3)))))

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
               (((l 3))
                new))
              (term
               (((l 3) (l1 Bot))
                l1)))
    
    (test-->> rr
              (term
               (((l 3) (l1 4))
                new))
              (term
               (((l 3) (l1 4) (l2 Bot))
                l2)))

    ;; E-PutVal
    (test-->> rr
              (term
               (((l Bot))
                (put l (3))))
              (term
               (((l 3))
                ())))
    
    (test-->> rr
              (term
               (((l 2))
                (put l (3))))
              (term
               (((l 3))
                ())))

    ;; This should work because put just puts the max of the current value and the new value.
    (test-->> rr
              (term
               (((l 2))
                (put l (1))))
              (term
               (((l 2))
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
               (((l Bot))
                l)))

    ;; let + E-New + E-PutVal + E-GetVal
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 (3))))
                    (let ((x_3 (get x_1 (2))))
                      x_3)))))
              (term
               (((l 3))
                (2))))
    
    ;; let par + E-New + E-PutVal + E-GetVal
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par ((x_2 (put x_1 (2)))
                            (x_3 (put x_1 (3))))
                    (get x_1 (2))))))
              (term
               (((l 3))
                (2))))

    ;; Another aspect of E-PutVal's behavior
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 (5))))
                    ;; This should just take the lub of the old and new
                    ;; values, i.e., 5.
                    (let ((x_3 (put x_1 (4))))
                      (get x_1 (5)))))))
              (term
               (((l 5))
                (5))))

    ;; E-PutValErr
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let ((x_2 (put x_1 (Top))))
                    x_2))))
              (term
               Error))

    (test-->> rr
              #:equiv cfgs-equal-modulo-perms?
              (term
               (()
                (let par ([x_1 new]
                          [x_2 new])
                  (let par ([x_3 (put x_1 (3))]
                            [x_4 (put x_2 (4))])
                    (get x_2 (4))))))

              ;; When we're using slow-rr, we can end up with
              ;; a store of ((l 3) (l1 4)) or a permutation thereof --
              ;; that is, x_1 is allocated first, followed by x_2.  When
              ;; we're using fast-rr, we always seem to get
              ;; the allocation in the opposite order.  This is not
              ;; nondeterministic in the sense that the result of
              ;; reading x_2 is always the same, but it ends up at a
              ;; different location in the store.  This is hack to
              ;; account for that.
              (if (equal? rr slow-rr)
                  (term
                   (((l 3)
                     (l1 4))
                    (4)))
                  (term
                   (((l 4)
                     (l1 3))
                    (4))))
              (term
               (((l 4)
                 (l1 3))
                (4))))
    
    ;;let par put and get
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par ((x_2 (put x_1 (2)))
                            (x_3 (get x_1 (2))))
                    (get x_1 (2))))))
              (term
               (((l 2))
                (2))))

    (test-results))

  ;; Warning: Passing `slow-rr` to this procedure will take
  ;; several orders of magnitude longer to finish than passing
  ;; `fast-rr`.
  (define (slow-program-test-suite rr)

    ;; let par + E-New + E-PutVal + E-GetVal + E-GetValBlock
    (test-->> rr
              (term
               (() ;; empty store
                (let ((x_1 new))
                  (let par
                      ;; Gets stuck trying to get 4 out of x_1, then
                      ;; unstuck after the other subexpression finishes.
                      ((x_4 (let par ((x_2 (put x_1 (2)))
                                      (x_3 (put x_1 (3))))
                              (get x_1 (4))))
                       ;; Eventually puts 4 in x_1 after several dummy
                       ;; beta-reductions.
                       (x_5 ((lambda (x_2)
                               ((lambda (x_2)
                                  ((lambda (x_2)
                                     ((lambda (x_2)
                                        ((lambda (x_2)
                                           (put x_1 (4))) ())) ())) ())) ())) ())))
                    x_4))))
              (term
               (((l 4))
                (4))))
    (test-results)))

(module test-fast racket
  (require (submod ".." test-suite))
  (test-fast))

(module test-all racket
  (require (submod ".." test-suite))
  (test-all))


