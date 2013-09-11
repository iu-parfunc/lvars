
;; Perform a simple test of the unique-desugaring transformation in figure 10 of this paper:
;; http://www.cs.indiana.edu/~lkuper/papers/lvars-fhpc13.pdf

;; This Scheme version is here in addition to the Haskell one because
;; it lets us do a convenient `eval`-based hack.

(load "pmatch.scm")

;; This transform only treats the core lambda calc.  It doesn't bother
;; with the new/put/get/consume cases.
(define (desugar p)
  (pmatch p
    ;; HINT - you can tweak some of these to 'trace-lambda's to watch
    ;; the pedigrees flow through:
    [(unique)               `(lambda (p ) (check-unique  p)  p)]
    [,n (guard (number? n)) `(lambda (_p) (check-unique _p) ,n)]
    [,v (guard (symbol? v)) `(lambda (_p) (check-unique _p) ,v)]
    [()                     `(lambda (_p) (check-unique _p) '())]
    [(,e1 ,e2)              `(lambda (p)  (check-unique p)
				     (((,(desugar e1) (cons 'L p))
				       (,(desugar e2) (cons 'R p)))
				      (cons 'J p)))]
    [(lambda (,v) ,e)       `(lambda (_p) (check-unique _p)
				     (lambda (,v) ,(desugar e)))]
    ))

;; This is a hack to make sure that we don't see the same pedigree
;; twice in ANY part of the program.
(define (check-unique p)
  (if (member p (all-observed))
      (error 'check-unique "Pedigree observed twice!!: ~s" p)
      (all-observed (cons p (all-observed)))))

;; To support aforementioned hack:
(define all-observed (make-parameter '()))

;;--------------------------------------------------------------------------------
;; Simple unit tests:

(define p0 `(lambda (x) x))
(define p1 `((lambda (x) x) 3))
(define p2 `(Sub1 3))
(define p3 `((times 3) 4))
(define p4 `(((is-zero 0) (lambda (_) 33)) (lambda (_) 44)))
(define p5 `(((is-zero 1) (lambda (_) 33)) (lambda (_) 44)))
(define p6 `((times (Sub1 3)) (Sub1 4)))
(define p7 `((lambda (_) (unique)) (unique)))
(define p8 `((lambda (x) x)        (unique)))
(define p9 `(((lambda (x) (lambda (y) x)) (unique)) (unique)))

(define fact '(lambda (fact)
		(lambda (n)
		  (((is-zero n)
		    (lambda (_) 1))
		   (lambda (_) ((times n) ((fact fact) (Sub1 n)))))
		  )))

(define p10 `((lambda (f) ((f f) 5)) ,fact))
(define (t10)  ((eval (wrap (desugar p10))) '()))

;--------------------------------------------------------------------------------

;; A pedigree which we do 
(define nullped 'PEDNOTUSED)

;; Provide definitions for some functions on numbers:
(define (wrap prog)
  `(let ((is-zero (lambda (n) 
		 (lambda (_ped1)
		   (lambda (f) 
		     (lambda (ped2)
		       (lambda (g) 
			 (if (zero? n) (f ped2) (g ped2)))))
		   )))
	 (times (lambda (n) 
		  (lambda (_)
		    (lambda (m) 
		      (lambda (_)			
			(* n m)
		      )))))

	 (Sub1 (lambda (n) (lambda (_) (sub1 n))))
	 )
     ,prog))

(define (test-all)
  (define (test prog) (all-observed '()) ((eval (wrap (desugar prog))) '()))  
  (map test (list p0 p1 p2 p3 p4 p5 
		  p6 p7 p8 p9 p10))
)


(display "Running all tests, results:\n")
(display (test-all))
