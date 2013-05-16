#lang racket

(require redex/reduction-semantics
         "LVish-nat.rkt")
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
   (term (store-dom ((l1 (4 #f)) (l2 (5 #f)) (l3 (Bot #f)))))
   (term (l1 l2 l3)))
  
  (test-equal
   (term (store-lookup ((l (2 #f))) l))
   (term 2))
  
  (test-equal
   (term (store-update () l 4))
   (term ((l (4 #f)))))
  
  (test-equal
   (term (store-update ((l (3 #f))) l 4))
   (term ((l (4 #f)))))

  (test-equal
   (term (store-update () l Bot))
   (term ((l (Bot #f)))))

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
                      (put l (3)))))
   (term (((l1 (Bot #f)))
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
              (put l (3))))
            (term
             (((l (3 #f)))
              ())))
  
  (test-->> rr
            (term
             (((l (2 #f)))
              (put l (3))))
            (term
             (((l (3 #f)))
              ())))

  ;; This should work because put just puts the max of the current value and the new value.
  (test-->> rr
            (term
             (((l (2 #f)))
              (put l (1))))
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

  ;; let + E-New + E-Put + E-Get + E-Convert
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let ((x_2 (put x_1 (3))))
                  (let ((x_3 (get x_1 (2))))
                    (convert x_3))))))
            (term
             (((l (3 #f)))
              (2))))
  
  ;; let par + E-New + E-Put + E-Get + E-Convert
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let par ((x_2 (put x_1 (2)))
                          (x_3 (put x_1 (3))))
                  (convert (get x_1 (2)))))))
            (term
             (((l (3 #f)))
              (2))))

  ;; Another aspect of E-Put's behavior
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let ((x_2 (put x_1 (5))))
                  ;; This should just take the lub of the old and new
                  ;; values, i.e., 5.
                  (let ((x_3 (put x_1 (4))))
                    (convert (get x_1 (5))))))))
            (term
             (((l (5 #f)))
              (5))))

  ;; E-Put-Err
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let ((x_2 (put x_1 (Top))))
                  (convert x_2)))))
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
                  (convert (get x_2 (4)))))))
            (term
             (((l (3 #f))
               (l1 (4 #f)))
              (4))))
  
  ;;let par + E-New + E-Put + E-Get
  (test-->> rr
            (term
             (() ;; empty store
              (let ((x_1 new))
                (let par ((x_2 (put x_1 (2)))
                          (x_3 (get x_1 (2))))
                  (convert (get x_1 (2)))))))
            (term
             (((l (2 #f)))
              (2))))

  ;; FIXME: this test gets stuck reducing.  Figure out what's wrong.
  ;; let par + E-New + E-Put + E-Get
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
                  (convert x_4)))))
            (term
             (((l (4 #f)))
              (4))))

  (test-results))

