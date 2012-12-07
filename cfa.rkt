#lang racket
(require "lambdaLVar.rkt"
         "lambdaLVar-stdlib.rkt"
         redex/reduction-semantics
         scheme/set)

;; Sketch of a 0CFA provided by Matt Might.  TODO: make this actually
;; work.

;; Code constructing the programs being analyzed is borrowed from
;; http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/k-CFA.ss

;; We're analyzing programs written in the CPS lambda calculus, which
;; has the following grammar:

;; exp  ::= (make-ref    <label> <var>)
;;       |  (make-lam    <label> (<var1> ... <varN>) <call>)
;; call ::= (make-call   <label> <exp0> <exp1> ... <expN>)

;; label = uninterned symbol

;; We use structs to represent the syntax of the programs being
;; analyzed:

(define-struct stx (label) #:prefab)
(define-struct (exp stx) () #:prefab)
(define-struct (ref exp) (var) #:prefab)
(define-struct (lam exp) (formals call) #:prefab)
(define-struct (call stx) (fun args) #:prefab)

;; And here are some helper functions for constructing syntax trees:

(define new-label gensym)

(define (make-ref* var) 
  (make-ref (new-label) var))

(define (make-lambda* formals call)
  (make-lam (new-label) formals call))

(define (make-call* fun . args)
  (make-call (new-label) fun args))

(define (make-let* var exp call)
  (make-call* (make-lambda* (list var) call) exp))

;; And here's a standard example program to be analyzed:

;; The Standard Example
;;
;; In direct style:
;;
;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
(define standard-example
  (make-let* 'id (make-lambda* '(x k) (make-call* (make-ref* 'k) (make-ref* 'x)))
             (make-call* (make-ref* 'id)
                         (make-lambda* '(z) (make-ref* 'z))
                         (make-lambda* '(a) 
                                       (make-call* (make-ref* 'id)
                                                   (make-lambda* '(y) (make-ref* 'y))
                                                   (make-lambda* '(b) 
                                                                 (make-ref* 'b)))))))

;; The tricky part here is going to be writing the analysis using only
;; the lambdaLVar language.



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





;; value = clo
;; For pure CPS, closures are the only kind of value.

;; clo ::= (make-closure <lambda> <benv>)
;; Closures pair a lambda term with a binding environment that
;; determines the value of its free variables.
(define-struct closure (lam benv) #:prefab)

;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are bindings.

;; bind ::= (make-binding <var> <time>)
;; A binding is minted each time a variable gets bound to a value.
(define-struct binding (var time) #:prefab)


;; Before the analysis begins, we need a mapping from variables in the
;; term being analyzed to store locations in the lambdaLVar store,
;; also known as a "binding environment".

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
