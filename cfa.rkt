#lang racket
(require redex/reduction-semantics
         "lambdaLVar.rkt")

;; Sketch of a 0CFA provided by Matt Might.  TODO: make this actually
;; work.

;; propagate :: Call* -> ()
;; propagate (call : rest) =
;;   let par _ = analyze(call) in
;;     propagate(rest)
;; propagate [] = ()
;;
;; analyze :: Call -> ()
;; analyze (Call f args)
;;   let par proc = eval(f) in
;;    let par argvals = map-par eval args in
;;     apply(proc,argvals)

;; apply :: Values x Values* -> ()
;; apply (procs, values) =
;;  let (p,rest) = choose (procs) in
;;   let par _ = applyProc(p,values) in
;;     apply(rest,values)

;; Values = Set Lambda

;; eval :: Exp -> Values
;; eval (Var v) = get (V(v))
;; eval (Lambda vars body) = { Lambda vars body }


;; applyProc :: Lambda x Values* -> ()
;; applyProc (Lambda vars body, argvals) =
;;  map-par (\ (var,val) ->
;;   put-join (V(var)) val)

;; main =
;;  while (changed())
;;    propagate(calls)

(define-metafunction lambdaLVar
  map-par : v (e ...) -> (e ...)
  [(map-par f (e_1 e_2 ...))
   (let par ((x_1 (f e_1))
             (x_2 (map-par f (e_2 ...))))
     ,(cons (term x_1)
            (term x_2)))])

(term
 (()
  ;; To write this program in lambdaLVar, first we need a Y combinator.
  (let ((Y
         (lambda (f)
           ((lambda (x) (f (lambda (v) ((x x) v))))
            (lambda (x) (f (lambda (v) ((x x) v))))))))
    ;; analyze :: Call -> ()
    (let ((analyze
           (Y (lambda (analyze)
                (lambda (call)))))))
    
    ;; propagate :: Call* -> ()
    (let ((propagate
           (Y (lambda (propagate)
                (lambda (calls)
                  ;; Escape to Scheme for 'cond'.
                  ,(cond
                     [(null? calls) (term '())]
                     [else
                      (term (let par ((x_1 (analyze (car calls)))
                                      (x_2 (propagate (cdr calls))))
                              ;; return value not important
                              '()))]))))))))))
