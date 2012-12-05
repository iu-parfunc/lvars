#lang racket
(require "lambdaLVar-stdlib.rkt")

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

;; analyze-call: Evaluate f, evaluate args, apply eval'd f to eval'd
;; args.
(define-metafunction lambdaLVar
  analyze-call : f (e ...) -> ()
  [(analyze-call f (e  ...))
   ;; Two or more args to be eval'd.
   (let par ((x_1 (eval f))
             (x_2 (map-par eval (e ...))))
     (apply x_1 x_2))])

(define-metafunction lambdaLVar
  ;; TODO.
  apply : v (v ...) -> ()
  )

(define-metafunction lambdaLVar
  eval : e -> v
  ;; TODO.
  [(eval x)
   ;; TODO: not sure if "get (V(v))" is supposed to be lambdaLVar
   ;; "get", or what V is supposed to be yet, exactly.

   ;; Hm.  V is "a mapping from variables in the program to store
   ;; locations."  "the program" is THE PROGRAM BEING ANALYZED.
   ()])

;; Representations of CPS'd lambda terms and binding environments
;; borrowed from example code at
;; http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/.

;; The analysis works on CPS'd lambda terms.

;; value = clo
;; For pure CPS, closures are the only kind of value.

;; clo ::= (make-closure <lambda> <benv>)
;; Closures pair a lambda term with a binding environment that
;; determinse the value of its free variables.
(define-struct closure (lam benv) #:prefab)

;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are bindings.

;; bind ::= (make-binding <var> <time>)
;; A binding is minted each time a variable gets bound to a value.
(define-struct binding (var time) #:prefab)


;; Before the analysis begins, we need a mapping from variables in the
;; term being analyzed to store locations, also known as a "binding
;; environment".

;; benv = hash[var,addr]
;; A binding environment maps variables to addresses.
(define empty-benv (make-immutable-hasheq empty))

; benv-lookup : benv var -> addr
(define benv-lookup hash-ref)

; benv-extend : benv var addr -> benv
(define benv-extend hash-set)

; benv-extend* : benv list[var] list[addr] -> benv
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))  




(term
 (()
  ;; To write this program in lambdaLVar, first we need a Y combinator.
  (let ((Y
         (lambda (f)
           ((lambda (x) (f (lambda (v) ((x x) v))))
            (lambda (x) (f (lambda (v) ((x x) v))))))))
    ;; analyze :: Call -> ()
    (let ((analyze (lambda (call)
                     (analyze-call (car call) (cdr call)))))

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
                              '()))])))))))))))
