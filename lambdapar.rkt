;; A Redex model of the lambdapar language.

#lang racket
(require redex)
(require srfi/1)

;; Export things we want to test.
(provide
 exists-d
 leq
 lub
 lubstore
 lubstore-helper
 rename-locs
 small-step-base-rr
 small-step-fast-rr
 store-dom
 store-dom-diff
 store-locs
 store-lookup
 store-top?
 store-update
 subst
 top?
 union
 valid)

;; We're assuming a domain where elements are of type `natural` for
;; simplicity.
(define leq-op <=)
(define lub-op max)

(define-language lambdapar
  ;; Configurations, on which the reduction relation is defined.
  (Config (S e) Error)
  
  ;; Expressions.
  (e x
     v
     (e e)
     (get e e)
     (put e e)
     new
     (convert e)

     ;; These don't appear in the grammar in the paper, because they
     ;; immediately desugar to application and lambda.
     (let ((x e)) e)
     (let par ((x e) (x e)) e))

  ;; Values.
  (v l
     Q
     (lambda (x) e))

  ;; Query set literals.  A query set is the set we pass to a `get`
  ;; expression that specifies a non-empty, pairwise incompatible
  ;; subset of the state space of the location being queried.  The
  ;; grammar allows empty query sets, as well, because those are the
  ;; return value of `put`.

  ;; Incidentally, under this grammar, (Top) and (Bot) are query
  ;; sets. (Bot) makes sense, but (Top) is nonsensical -- a program
  ;; that queried a variable for (Top) would block forever.
  ;; Nevertheless, the grammar admits it.
  (Q (d ...))
  ;; TODO: support for { d | pred(d) }-style query sets (issue #5).
  
  ;; Stores.  A store is either a finite partial mapping from
  ;; locations to domain values (excluding Top), or it is the
  ;; distinguished element TopS.
  (S ((l StoreVal) ...) TopS)

  ;; Domains contain elements to which locations can be bound.  We
  ;; assume a domain of naturals (plus Top and Bot) for now.
  (d Top StoreVal)
  (StoreVal natural Bot)

  ;; Ranges of a couple of metafunctions.
  (d/lookupfailed d lookupfailed)
  (S/updatefailed S updatefailed)
  (Bool #t #f)
  (d/Bool d Bool)

  (x variable-not-otherwise-mentioned)
  (l variable-not-otherwise-mentioned))

;; Because our evaluation relation is defined using inference rules
;; rather than with evaluation contexts, we have to use Redex's
;; define-judgment-form feature rather than the reduction-relation
;; feature.  (In order to take the evaluation context approach, we
;; would need multiple-hole contexts, which Redex doesn't support.)
;; We can, however, wrap the small-step relation in
;; 'reduction-relation'.

(define small-step-base-rr
  (reduction-relation
   lambdapar
   (--> Config_1 Config_2
        (judgment-holds (small-step-base Config_1 Config_2)))))

(define small-step-fast-rr
  (reduction-relation
   lambdapar
   (--> Config_1 Config_2
        (judgment-holds (small-step-fast Config_1 Config_2)))))

(define trace-base
  (lambda (config)
    (traces small-step-base-rr config)))

(define trace-fast
  (lambda (config)
    (traces small-step-fast-rr config)))

;; NB: in Racket v5.2, `define-judgment-form` doesn't let you use
;; `side-condition`; you have to use `where`, which does pattern
;; matching.  So for now, we have to define metafunctions for all our
;; side conditions and then pattern-match with `(where #t (call to
;; metafunction ...))`.  This should be fixed in the most recent
;; release, which I'm having trouble installing!

;; Reduction rules shown in Figure 3 of the paper, minus E-Refl and
;; E-ReflErr, and with the addition of the rules shown in Figure 5.
(define-judgment-form lambdapar
  #:mode (small-step-base I O)
  #:contract (small-step-base Config Config)

  ;; E-App-1 (only reduce left term)
  [(small-step-base (S (e_1 e_2))
                    (S_1 (e_11 e_2)))
   (small-step-base (S e_1) (S_1 e_11))
   (where #f (store-top? (lubstore S S_1)))]

  ;; E-App-2 (only reduce right term)
  [(small-step-base (S (e_1 e_2))
                    (S_2 (e_1 e_22)))
   (small-step-base (S e_2) (S_2 e_22))
   (where #f (store-top? (lubstore S S_2)))]

  ;; E-ParApp
  [(small-step-base (S (e_1 e_2))
                    ((lubstore S_11 S_2) (e_111 e_22)))
   (small-step-base (S e_1) (S_1 e_11))
   (small-step-base (S e_2) (S_2 e_22))
   ;; Handles renaming: any new locations created between S and S_1
   ;; must have names unique from those created between S and S_2.
   ;; See `rename-locs` for more details.
   (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
   (where #f (store-top? (lubstore S_11 S_2)))]

  ;; E-Beta
  [(small-step-base (S ((lambda (x) e) v))
                    (S (subst x v e)))]

  ;; E-New
  [(small-step-base (S new)
                    ((store-update S l Bot) l))
   (where l (variable-not-in-store S))]

  ;; E-Put-1
  [(small-step-base (S (put e_1 e_2))
                    (S_1 (put e_11 e_2)))
   (small-step-base (S e_1) (S_1 e_11))]

  ;; E-Put-2
  [(small-step-base (S (put e_1 e_2))
                    (S_2 (put e_1 e_22)))
   (small-step-base (S e_2) (S_2 e_22))]

  ;; E-PutVal
  [(small-step-base (S (put l (d_2)))
                    ((store-update S l d_2) ()))
   (where d_1 (store-lookup S l))
   (where #f (top? (lub d_1 d_2)))]

  ;; E-Get-1
  [(small-step-base (S (get e_1 e_2))
                    (S_1 (get e_11 e_2)))
   (small-step-base (S e_1) (S_1 e_11))]

  ;; E-Get-2
  [(small-step-base (S (get e_1 e_2))
                    (S_2 (get e_1 e_22)))
   (small-step-base (S e_2) (S_2 e_22))]

  ;; E-GetVal
  [(small-step-base (S (get l Q))
                    (S (d_2)))
   (where d_1 (store-lookup S l))
   (where #t (incomp Q))
   (where #t (valid Q))
   (where d_2 (exists-d d_1 Q))]

  ;; E-Convert
  [(small-step-base (S_1 (convert e_1))
                    (S_2 (convert e_2)))
   (small-step-base (S_1 e_1) (S_2 e_2))]

  ;; E-ConvertVal
  [(small-step-base (S (convert Q))
                    (S (delta Q)))]

  ;; Desugaring of lets.
  ;; TODO: multiple bindings? (issue #7)
  [(small-step-base (S (let ((x_1 e_1)) e_2))
                    (S ((lambda (x_1) e_2) e_1)))]

  [(small-step-base (S (let par ((x_1 e_1) (x_2 e_2)) e_3))
                    (S (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))]

  ;; E-AppErr-1
  [(small-step-base (S (e_1 e_2))
                    Error)
   (small-step-base (S e_1) Error)]

  ;; E-AppErr-2
  [(small-step-base (S (e_1 e_2))
                    Error)
   (small-step-base (S e_2) Error)]

  ;; E-ParAppErr
  [(small-step-base (S (e_1 e_2))
                    Error)
   (small-step-base (S e_1) (S_1 e_11))
   (small-step-base (S e_2) (S_2 e_22))
   (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
   (where #t (store-top? (lubstore S_11 S_2)))]

  ;; E-PutValErr
  [(small-step-base (S (put l (d_2)))
                    Error)
   (where d_1 (store-lookup S l))
   (where #t (top? (lub d_1 d_2)))]

  ;; E-ConvertErr
  [(small-step-base (S (convert e))
                    Error)
   (small-step-base (S e) Error)])

;; Reduction rules shown in Figure 3 of the paper, minus E-Refl and
;; E-ReflErr, and with the addition of the rules shown in Figure 6.
;; Much faster in Redex than `small-step-base`.
(define-judgment-form lambdapar
  #:mode (small-step-fast I O)
  #:contract (small-step-fast Config Config)

  ;; E-App-1 (only reduce left term; right term is a value)
  [(small-step-fast (S (e_1 v))
                    (S_1 (e_11 v)))
   (small-step-fast (S e_1) (S_1 e_11))
   (where #f (store-top? (lubstore S S_1)))]

  ;; E-App-2 (only reduce right term; left term is a value)
  [(small-step-fast (S (v e_2))
                    (S_2 (v e_22)))
   (small-step-fast (S e_2) (S_2 e_22))
   (where #f (store-top? (lubstore S S_2)))]

  ;; E-GetValBlock
  [(small-step-fast (S (get l Q))
                    (S (get l Q)))
   (where d_1 (store-lookup S l))
   (where #t (incomp Q))
   (where #t (valid Q))
   (where #f (exists-d d_1 Q))]

  ;; The following rules are identical to the ones of the same names
  ;; in `small-step-base` (more's the pity).  It would be great if
  ;; Redex had an `extend-judgment-form` form, analogous to
  ;; `extend-reduction-relation`, to avoid having to repeat these
  ;; rules here!
  
  ;; E-ParApp
  [(small-step-fast (S (e_1 e_2))
                    ((lubstore S_11 S_2) (e_111 e_22)))
   (small-step-fast (S e_1) (S_1 e_11))
   (small-step-fast (S e_2) (S_2 e_22))
   ;; Handles renaming: any new locations created between S and S_1
   ;; must have names unique from those created between S and S_2.
   ;; See `rename-locs` for more details.
   (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
   (where #f (store-top? (lubstore S_11 S_2)))]

  ;; E-Beta
  [(small-step-fast (S ((lambda (x) e) v))
                    (S (subst x v e)))]

  ;; E-New
  [(small-step-fast (S new)
                    ((store-update S l Bot) l))
   (where l (variable-not-in-store S))]

  ;; E-Put-1
  [(small-step-fast (S (put e_1 e_2))
                    (S_1 (put e_11 e_2)))
   (small-step-fast (S e_1) (S_1 e_11))]

  ;; E-Put-2
  [(small-step-fast (S (put e_1 e_2))
                    (S_2 (put e_1 e_22)))
   (small-step-fast (S e_2) (S_2 e_22))]

  ;; E-PutVal
  [(small-step-fast (S (put l (d_2)))
                    ((store-update S l d_2) ()))
   (where d_1 (store-lookup S l))
   (where #f (top? (lub d_1 d_2)))]

  ;; E-Get-1
  [(small-step-fast (S (get e_1 e_2))
                    (S_1 (get e_11 e_2)))
   (small-step-fast (S e_1) (S_1 e_11))]

  ;; E-Get-2
  [(small-step-fast (S (get e_1 e_2))
                    (S_2 (get e_1 e_22)))
   (small-step-fast (S e_2) (S_2 e_22))]

  ;; E-GetVal
  [(small-step-fast (S (get l Q))
                    (S (d_2)))
   (where d_1 (store-lookup S l))
   (where #t (incomp Q))
   (where #t (valid Q))
   (where d_2 (exists-d d_1 Q))]

  ;; E-Convert
  [(small-step-fast (S_1 (convert e_1))
                    (S_2 (convert e_2)))
   (small-step-fast (S_1 e_1) (S_2 e_2))]

  ;; E-ConvertVal
  [(small-step-fast (S (convert Q))
                    (S (delta Q)))]

  ;; Desugaring of lets.
  [(small-step-fast (S (let ((x_1 e_1)) e_2))
                    (S ((lambda (x_1) e_2) e_1)))]

  [(small-step-fast (S (let par ((x_1 e_1) (x_2 e_2)) e_3))
                    (S (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))]

  ;; E-AppErr-1
  [(small-step-fast (S (e_1 e_2))
                    Error)
   (small-step-fast (S e_1) Error)]

  ;; E-AppErr-2
  [(small-step-fast (S (e_1 e_2))
                    Error)
   (small-step-fast (S e_2) Error)]

  ;; E-ParAppErr
  [(small-step-fast (S (e_1 e_2))
                    Error)
   (small-step-fast (S e_1) (S_1 e_11))
   (small-step-fast (S e_2) (S_2 e_22))
   (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
   (where #t (store-top? (lubstore S_11 S_2)))]

  ;; E-PutValErr
  [(small-step-fast (S (put l (d_2)))
                    Error)
   (where d_1 (store-lookup S l))
   (where #t (top? (lub d_1 d_2)))]

  ;; E-ConvertErr
  [(small-step-fast (S (convert e))
                    Error)
   (small-step-fast (S e) Error)])

(define-metafunction lambdapar
  store-dom : S -> (l ...)
  [(store-dom ()) ()]
  [(store-dom ((l_1 d_1) (l_2 d_2) ...))
   ,(cons (term l_1) (term (store-dom ((l_2 d_2) ...))))])

;; Return a list of locations in dom(S_1) that are not in dom(S_2).
(define-metafunction lambdapar
  store-dom-diff : S S -> (l ...)
  [(store-dom-diff S_1 S_2)
   ,(lset-difference equal?
                     (term (store-dom S_1))
                     (term (store-dom S_2)))])

;; Rename locations so threads don't conflict.
(define-metafunction lambdapar
  rename-locs : (S e) S S -> (S e)
  [(rename-locs (S_1 e_11) S_2 S)
   ;; Any new locations created between S and S_1 need to be given
   ;; names unique from those created between S and S_2.  That's what
   ;; this does.

   ;; For each location created between S and S_1 (that is,
   ;; (store-dom-diff S_1 S)), we generate a fresh variable not
   ;; appearing in the set of locations created between S and S_2
   ;; (that is, (store-dom-diff S_2 S)) and capture-avoidingly
   ;; substitute it into the configuration (S_1 e_11).
   ,(fold-right
     (lambda (loc config)
       (term (subst
              ,loc
              ,(variable-not-in (term (store-dom-diff S_2 S))
                                (term ,loc))
              ,config)))
     (term (S_1 e_11))
     (term (store-dom-diff S_1 S)))])

;; The greatest element of the store lattice is any store in which
;; some location is bound to Top.
(define-metafunction lambdapar
  store-top? : S -> Bool
  [(store-top? TopS) ,#t]
  [(store-top? S) ,#f])

(define-metafunction lambdapar
  top? : d -> Bool
  [(top? Top) ,#t]
  [(top? d) ,#f])

(define-metafunction lambdapar
  lub : d d -> d
  [(lub d_1 d_2) d_2
   (where #t (leq d_1 d_2))]
  [(lub d_1 d_2) d_1
   (where #t (leq d_2 d_1))]
  [(lub d_1 d_2) ,(lub-op (term d_1) (term d_2))])

(define-metafunction lambdapar
  leq : d d -> Bool
  [(leq Bot d_2) ,#t]
  [(leq d_1 Bot) ,#f]
  [(leq Top d_2) ,#f]
  [(leq d_1 Top) ,#t]
  [(leq d_1 d_2) ,(leq-op (term d_1) (term d_2))])

(define-metafunction lambdapar
  lubstore : S S -> S
  [(lubstore S_1 ()) S_1]
  [(lubstore () S_2) S_2]
  [(lubstore S_1 S_2)
   ;; Get the union of labels from S_1 and S_2
   ,(let* ([union-locs (term (union (store-locs S_1)
                                    (store-locs S_2)))]
           ;; For each label in the list, take the lub of S_1(l) and S_2(l)
           [union-lubs (term ,(map (lambda (loc)
                                     (term (lubstore-helper S_1 S_2 ,loc)))
                                   (term ,union-locs)))])
      ;; Put labels back together with their lubs.
      (zip union-locs union-lubs))])

;; Given a store location `l` and two stores `S_1` and `S_2`, return
;; the lub of S_1(l) and S_2(l).  We know that every l this function
;; gets is going to be in the domain of either S_1 or S_2 or both.

;; TODO: could use style improvement with `where` clauses (issue #8).
(define-metafunction lambdapar
  lubstore-helper : S S l -> d
  [(lubstore-helper S_1 S_2 l)
   ,(let ([d_1 (term (store-lookup S_1 l))]
          [d_2 (term (store-lookup S_2 l))])
      (cond
        [(equal? d_1 (term lookupfailed)) d_2]
        [(equal? d_2 (term lookupfailed)) d_1]
        [else (term (lub ,d_1 ,d_2))]))])

;; Helper function to get all the locations in a store
(define-metafunction lambdapar
  store-locs : S -> (l ...)
  [(store-locs ()) ()]
  [(store-locs ((l d) (l_1 d_1) ...))
   ,(cons (term l)
          (term (store-locs ((l_1 d_1) ...))))])

;; Helper function to take the union of lists of locations
(define-metafunction lambdapar
  union : (l ...) (l ...) -> (l ...)
  [(union () (l ...)) (l ...)]
  [(union (l ...) ()) (l ...)]
  [(union (l_1 l_2 ...) (l ...))
   ,(if (memq (term l_1) (term (l ...)))
        (term (union (l_2 ...) (l ...)))
        (cons (term l_1)
              (term (union (l_2 ...) (l ...)))))])

(define-metafunction lambdapar
  variable-not-in-store : S -> l
  [(variable-not-in-store S)
   ,(variable-not-in (term S) (term l))])

(define-metafunction lambdapar
  store-lookup : S l -> d/lookupfailed
  ;; TODO: more Redex-y way to express this? (issue #9)
  [(store-lookup S l) ,(let ([v (assq (term l) (term S))])
                         (if v
                             (term ,(second v))
                             (term lookupfailed)))])

;; Actually handles both updates and extensions.
(define-metafunction lambdapar
  store-update : S l d -> S/updatefailed
  [(store-update () l d) ((l d))
   ;; TODO: deal with case where d = Top, or tighten up to not accept
   ;; Top (issue #10)
   ]
  [(store-update ((l_2 d_2) (l_3 d_3) ...) l d)
   ;; TODO: deal with case where (lub d_1 d_2) = Top (issue #10)
   ,(if (equal? (term l) (term l_2))
        (cons (term (l_2 (lub d d_2)))
              (term ((l_3 d_3) ...)))
        (cons (term (l_2 d_2))
              (term (store-update ((l_3 d_3) ...) l d))))])

;; The second condition on the E-GetVal rule.  For any two distinct
;; elements in Q, the lub of them is Top.
(define-metafunction lambdapar
  incomp : Q -> Bool
  [(incomp ()) ,#t]
  [(incomp (d)) ,#t]
  ;; TODO: deal with case where d = Top or Bot (issue #10)
  [(incomp (d_1 d_2)) ,(equal? (term (lub d_1 d_2)) (term Top))]
  ;; Something like this?
  [(incomp (d_1 d_2 d_3 ...))
   ,(and (equal? (term (lub d_1 d_2)) (term Top))
         (term (incomp (d_1 d_3 ...)))
         (term (incomp (d_2 d_3 ...))))])

;; The third condition on the E-GetVal rule.
(define-metafunction lambdapar
  valid : Q -> Bool
  [(valid Q) ,(not (null? (term Q)))])

;; The fourth condition on the E-GetVal rule.
(define-metafunction lambdapar
  exists-d : d Q -> d/Bool
  [(exists-d d_1 ()) #f]
  ;; 
  [(exists-d d_1 (d_2 d_3 ...)) #f
   (where #f (leq d_2 d_1))]
  [(exists-d d_1 (d_2 d_3 ...)) d_2
   (where #t (leq d_2 d_1))]
  [(exists-d d_1 (d_2 d_3 ...))
   (exists-d d_1 (d_3 ...))])

(define-metafunction lambdapar
  delta : Q -> Q
  ;; Let's just have it be identity for now...
  [(delta Q) Q])

;; subst and subst-vars: capture-avoiding substitution, due to
;; redex.racket-lang.org/lam-v.html.

(define-metafunction lambdapar
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in lambda body
  [(subst x_1 any_1 (lambda (x_2 ... x_1 x_3 ...) any_2))
   (lambda (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]

  ;; 2. general purpose capture-avoiding case
  [(subst x_1 any_1 (lambda (x_2 ...) any_2))
   (lambda (x_new ...)
     (subst x_1 any_1 (subst-vars (x_2 x_new) ... any_2)))
   (where (x_new ...) ,(variables-not-in (term (x_1 any_1 any_2))
                                           (term (x_2 ...))))]

  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]

  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]

  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction lambdapar
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...))
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3)
   (subst-vars (x_1 any_1)
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])
