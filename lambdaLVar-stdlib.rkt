;; The lambdaLVar "standard" "library".

;; This file contains stuff that, hopefully, makes it marginally less
;; painful to write programs in lambdaLVar.

#lang racket
(require redex/reduction-semantics
         "lambdaLVar.rkt")

;; map-par: Apply a function in parallel to all items of a list.  This
;; is a little strange, since lambdaLVar doesn't actually have lists.
;; But, hey, we'll give it a shot.  "v" is the function being mapped.
(define-metafunction lambdaLVar
  map-par : v (e ...) -> (e ...)
  ;; If the list is empty, there's nothing to do.
  [(map-par v ()) ()]
  ;; If the list has one thing in it, there's nothing to parallelize.
  [(map-par v (e_1))
   ,(list (term (v e_1)))]
  ;; If the list has two or more things, apply v to the first, and in
  ;; parallel, map-par over the rest; then assemble the results.
  [(map-par v (e_1 e_2 ...))
   (let par ((x_1 (v e_1))
             (x_2 (map-par v (e_2 ...))))
     ,(cons (term x_1) (term x_2)))])
