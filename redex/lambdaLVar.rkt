#lang racket
;; A Redex model of the lambdaLVar language.  Mentions of "the TR" in
;; this document refer to
;; http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR702

(provide define-lambdaLVar-language)

(define-syntax-rule (define-lambdaLVar-language name lub-op lattice-values ...)
  (begin
    (require redex/reduction-semantics)
    (require srfi/1)

    (provide slow-rr
             fast-rr
             exists-d
             lub
             leq
             lubstore
             lubstore-helper
             store-dom
             store-lookup
             store-update
             incomp
             valid
             store-dom-diff
             store-top?
             top?
             subst
             rename-locs)
    
    (define-language name
      ;; Configurations, on which the reduction relation is defined.
      (Config (S e) Error)
      
      ;; Expressions.
      (e x
         v
         (e e)
         (get e e)
         (put e e)
         new

         ;; These don't appear in the grammar in the TR, because they
         ;; immediately desugar to application and lambda.
         (let ((x e)) e)
         (let par ((x e) (x e)) e))

      ;; Values.
      (v l
         Q
         (lambda (x) e))

      ;; Threshold set literals.  A threshold set is the set we pass to a
      ;; `get` expression that specifies a non-empty, pairwise
      ;; incompatible subset of the state space of the location being
      ;; queried.  The grammar allows empty threshold sets, as well,
      ;; because those are the return value of `put`.

      ;; Incidentally, under this grammar, (Top) and (Bot) are threshold
      ;; sets. (Bot) makes sense, but (Top) is nonsensical -- a program
      ;; that passed (Top) as a threshold would block forever.
      ;; Nevertheless, the grammar admits it.
      (Q (d (... ...)))

      ;; Stores.  A store is either a set of LVars (that is, a finite
      ;; partial mapping from locations to StoreVals) or a
      ;; distinguished value TopS.
      (S (LVar (... ...)) TopS)
      (LVar (l StoreVal))

      ;; Lattice elements, representing the state of an LVar.  We
      ;; assume Top and Bot lattice elements in addition to the
      ;; user-specified lattice-values. A StoreVal can be any element
      ;; of the lattice except Top (see Definition 1 in the TR).
      (d Top StoreVal)
      (StoreVal lattice-values ... Bot)

      ;; Ranges of a couple of metafunctions.
      (StoreVal/lookupfailed StoreVal lookupfailed)
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

    (define slow-rr
      (reduction-relation
       name
       (--> Config_1 Config_2
            (judgment-holds (small-step-slow Config_1 Config_2)))))

    (define fast-rr
      (reduction-relation
       name
       (--> Config_1 Config_2
            (judgment-holds (small-step-fast Config_1 Config_2)))))

    ;; NB: in Racket v5.2, `define-judgment-form` doesn't let you use
    ;; `side-condition`; you have to use `where`, which does pattern
    ;; matching.  So for now, we have to define metafunctions for all our
    ;; side conditions and then pattern-match with `(where #t (call to
    ;; metafunction ...))`.  This should be fixed in the most recent
    ;; release.

    ;; Reduction rules shown in Figure 4 of the TR, minus E-Refl and
    ;; E-ReflErr.  We'll patch the missing reflexive rules in the extended
    ;; reduction relations defined below this one.
    (define-judgment-form name
      #:mode (small-step-base I O)
      #:contract (small-step-base Config Config)

      [(small-step-base (S (e_1 e_2))
                        ((lubstore S_11 S_2) (e_111 e_22)))
       (small-step-base (S e_1) (S_1 e_11))
       (small-step-base (S e_2) (S_2 e_22))
       ;; Handles renaming: any new locations created between S and S_1
       ;; must have names unique from those created between S and S_2.
       ;; See `rename-locs` for more details.
       (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
       (where #f (store-top? (lubstore S_11 S_2)))
       "E-ParApp"]

      [(small-step-base (S ((lambda (x) e) v))
                        (S (subst x v e)))
       "E-Beta"]
      
      [(small-step-base (S new)
                        ((store-update S l Bot) l))
       (where l (variable-not-in-store S))
       "E-New"]

      [(small-step-base (S (put e_1 e_2))
                        (S_1 (put e_11 e_2)))
       (small-step-base (S e_1) (S_1 e_11))
       "E-Put-1"]

      [(small-step-base (S (put e_1 e_2))
                        (S_2 (put e_1 e_22)))
       (small-step-base (S e_2) (S_2 e_22))
       "E-Put-2"]

      [(small-step-base (S (put l (d_2)))
                        ((store-update S l d_2) ()))
       (where d_1 (store-lookup S l))
       (where #f (top? (lub d_1 d_2)))
       "E-PutVal"]

      [(small-step-base (S (get e_1 e_2))
                        (S_1 (get e_11 e_2)))
       (small-step-base (S e_1) (S_1 e_11))
       "E-Get-1"]

      [(small-step-base (S (get e_1 e_2))
                        (S_2 (get e_1 e_22)))
       (small-step-base (S e_2) (S_2 e_22))
       "E-Get-2"]

      [(small-step-base (S (get l Q))
                        (S (d_1)))
       (where d_2 (store-lookup S l))
       (where #t (incomp Q))
       (where #t (valid Q))
       (where d_1 (exists-d d_2 Q))
       "E-GetVal"]


      [(small-step-base (S (let ((x_1 e_1)) e_2))
                        (S ((lambda (x_1) e_2) e_1)))
       "Desugaring of let"]

      [(small-step-base (S (let par ((x_1 e_1) (x_2 e_2)) e_3))
                        (S (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))
       "Desugaring of let par"]

      [(small-step-base (S (e_1 e_2))
                        Error)
       (small-step-base (S e_1) Error)
       "E-AppErr-1"]

      [(small-step-base (S (e_1 e_2))
                        Error)
       (small-step-base (S e_2) Error)
       "E-AppErr-2"]

      [(small-step-base (S (e_1 e_2))
                        Error)
       (small-step-base (S e_1) (S_1 e_11))
       (small-step-base (S e_2) (S_2 e_22))
       (where (S_11 e_111) (rename-locs (S_1 e_11) S_2 S))
       (where #t (store-top? (lubstore S_11 S_2)))
       "E-ParAppErr"]

      [(small-step-base (S (put l (d_2)))
                        Error)
       (where d_1 (store-lookup S l))
       (where #t (top? (lub d_1 d_2)))
       "E-PutValErr"])

    ;; Because we left out the E-Refl and E-ReflErr rules from our
    ;; semantics, we have to add two new rules, E-App-1 and E-App-2, by
    ;; which parallel application expressions may take a step even if only
    ;; one of their subexpressions can take a step.  This is the simplest
    ;; way we know of to patch the missing reflexive rules, but
    ;; unfortunately, it runs really slowly Hence the name of this
    ;; relation.
    (define-extended-judgment-form name small-step-base
      #:mode (small-step-slow I O)
      #:contract (small-step-slow Config Config)

      [(small-step-slow (S (e_1 e_2)) (S_1 (e_11 e_2)))
       (small-step-slow (S e_1) (S_1 e_11))
       (where #f (store-top? (lubstore S S_1)))
       "E-App-1"] ;; only reduce left term

      [(small-step-slow (S (e_1 e_2))
                        (S_2 (e_1 e_22)))
       (small-step-slow (S e_2) (S_2 e_22))
       (where #f (store-top? (lubstore S S_2)))
       "E-App-2"] ;; only reduce right term
      )

    ;; In this version, we again patch the missing reflexive rules, but
    ;; this time, in E-App-1 and E-App-2, the subexpression that is not
    ;; taking a step must be a value.  We also add an E-GetValBlock rule,
    ;; which allows a blocked get expression to step to itself. This is
    ;; This version is much faster in Redex than `small-step-slow`.
    (define-extended-judgment-form name small-step-base
      #:mode (small-step-fast I O)
      #:contract (small-step-fast Config Config)

      [(small-step-fast (S (e_1 v))
                        (S_1 (e_11 v)))
       (small-step-fast (S e_1) (S_1 e_11))
       (where #f (store-top? (lubstore S S_1)))
       "E-App-1"] ;; only reduce left term; right term is a value

      [(small-step-fast (S (v e_2))
                        (S_2 (v e_22)))
       (small-step-fast (S e_2) (S_2 e_22))
       (where #f (store-top? (lubstore S S_2)))
       "E-App-2"] ;; only reduce right term; left term is a value

      ;; Premises 1, 2, and 3 of E-GetVal hold, but there does not exist a
      ;; d_1 in Q such that (leq d_1 d_2), so premises 4 and 5 do not
      ;; (both) hold.
      [(small-step-fast (S (get l Q))
                        (S (get l Q)))
       (where d_2 (store-lookup S l))
       (where #t (incomp Q))
       (where #t (valid Q))
       (where #f (exists-d d_2 Q))
       "E-GetValBlock"])

    ;; Some convenience functions: LVar accessors and constructor.

    (define-metafunction name
      lvloc : LVar -> l
      [(lvloc LVar) ,(first (term LVar))])

    (define-metafunction name
      lvstate : LVar -> StoreVal
      [(lvstate LVar) ,(second (term LVar))])

    (define-metafunction name
      build-lv : l StoreVal -> LVar
      [(build-lv l StoreVal)
       (l StoreVal)])
    
    (define-metafunction name
      store-dom : S -> (l (... ...))
      [(store-dom ()) ()]
      [(store-dom ((l_1 StoreVal_1) (l_2 StoreVal_2) (... ...)))
       ,(cons (term l_1) (term (store-dom ((l_2 StoreVal_2) (... ...)))))])

    ;; Return a list of locations in dom(S_1) that are not in dom(S_2).
    (define-metafunction name
      store-dom-diff : S S -> (l (... ...))
      [(store-dom-diff S_1 S_2)
       ,(lset-difference equal?
                         (term (store-dom S_1))
                         (term (store-dom S_2)))])

    ;; Rename locations so threads don't conflict.
    (define-metafunction name
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
    (define-metafunction name
      store-top? : S -> Bool
      [(store-top? TopS) ,#t]
      [(store-top? S) ,#f])

    (define-metafunction name
      top? : d -> Bool
      [(top? Top) ,#t]
      [(top? d) ,#f])

    ;; N.B.: The lub of d_1 and d_2 is the element d_3 such that:
    ;; -- (leq d_1 d_3)
    ;; -- (leq d_2 d_3)
    ;; -- for all d_4 s.t. (leq d_1 d_4) and (leq d_2 d_4), (leq d_3 d_4).
    ;;
    ;; But we can't get Redex to compute that, so instead, we ask the user
    ;; to provide lub, then compute leq in terms of lub.
    ;;
    ;; Intended to be extended by a user-provided metafunction/extension.
    (define-metafunction name
      lub : d d -> d
      [(lub Bot d_2) d_2]
      [(lub d_1 Bot) d_1]
      [(lub Top d_2) Top]
      [(lub d_1 Top) Top]
      [(lub d_1 d_2) ,(lub-op (term d_1) (term d_2))])

    ;; Defined in terms of lub.
    (define-metafunction name
      leq : d d -> Bool
      [(leq Bot d_2) ,#t]
      [(leq d_1 Bot) ,#f]
      [(leq Top d_2) ,#f]
      [(leq d_1 Top) ,#t]

      ;; If d_1 = d_2, then (leq d_1 d_2).
      [(leq d_1 d_2) ,#t
       (side-condition (equal? (term d_1) (term d_2)))]

      ;; If (lub d_1 d_2) = d_2, then (leq d_1 d_2).
      [(leq d_1 d_2) ,#t
       (side-condition (equal? (term (lub d_1 d_2)) (term d_2)))]

      ;; If (lub d_1 d_2) = d_1, then (not (leq d_1 d_2)).  (This assumes
      ;; that d_1 != d_2, but we've already covered the case where they're
      ;; equal.)
      [(leq d_1 d_2) ,#f
       (side-condition (equal? (term (lub d_1 d_2)) (term d_1)))]

      ;; The only case left: (lub d_1 d_2) = d_3, where d_3 is greater
      ;; than both d_1 and d_2.  In this case, (not (leq d_1 d_2)).
      [(leq d_1 d_2) ,#f])

    ;; Definition 3 in the TR.
    (define-metafunction name
      lubstore : S S -> S
      [(lubstore S_1 ()) S_1]
      [(lubstore () S_2) S_2]

      ;; The TopS case.
      [(lubstore S_1 S_2)
       TopS
       (where #t (lubstore-TopS? S_1 S_2))]

      ;; Otherwise, (lubstore S_1 S_2) is the store S such that (store-dom
      ;; S) = (union (store-dom S_1) (store-dom S_2)), and, for all l in
      ;; (store-dom S),

      ;; S(l) = (lub S_1(l) S_2(l))
      ;;        if (member? l (intersection (store-dom S_1) (store_dom S_2)))
      ;; S(l) = S_1(l) if (not (member? l (store-dom S_2)))
      ;; S(l) = S_2(l) if (not (member? l (store-dom S_1)))

      [(lubstore S_1 S_2)
       ;; Get the union of labels from S_1 and S_2
       ,(let* ([locs (lset-union equal?
                                 (term (store-dom S_1))
                                 (term (store-dom S_2)))]
               ;; For each label in the list, take the lub of S_1(l) and S_2(l),
               [lubs (term ,(map (lambda (loc)
                                   (term (lubstore-helper S_1 S_2 ,loc)))
                                 locs))])
          ;; Put labels back together with their lubs.
          (zip locs lubs))])

    (define-metafunction name
      lubstore-TopS? : S S -> Bool
      [(lubstore-TopS? S_1 S_2)
       ;; (lubstore-TopS? S_1 S_2) == #t iff there exists some l in
       ;; (intersection (store-dom S_1) (store-dom S_2)) such that (lub
       ;; (store-lookup S_1 l) (store-lookup S_2 l)) == Top.
       
       ;; First, get the intersection of the domains of S_1 and S_2.
       ,(let* ([locs (lset-intersection equal?
                                        (term (store-dom S_1))
                                        (term (store-dom S_2)))]
               ;; For each such label l, take the lub of S_1(l) and S_2(l).
               [lubs (term ,(map (lambda (loc)
                                   (term (lubstore-helper S_1 S_2 ,loc)))
                                 locs))])
          ;; If any lub in the resulting list is Top, return #t;
          ;; otherwise, return #f.
          (if (member (term Top) lubs)
              #t
              #f))])

    ;; Given a store location `l` and two stores `S_1` and `S_2`, return
    ;; the lub of S_1(l) and S_2(l).  We know that every l this function
    ;; gets is going to be in the domain of either S_1 or S_2 or both.
    (define-metafunction name
      lubstore-helper : S S l -> d
      [(lubstore-helper S_1 S_2 l)
       ,(let ([d_1 (term (store-lookup S_1 l))]
              [d_2 (term (store-lookup S_2 l))])
          (cond
            [(equal? d_1 (term lookupfailed)) d_2]
            [(equal? d_2 (term lookupfailed)) d_1]
            [else (term (lub ,d_1 ,d_2))]))])

    (define-metafunction name
      variable-not-in-store : S -> l
      [(variable-not-in-store S)
       ,(variable-not-in (term S) (term l))])

    (define-metafunction name
      store-lookup : S l -> StoreVal/lookupfailed
      [(store-lookup S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvstate ,lv))
                                 (term lookupfailed)))])

    ;; Actually handles both updates and extensions.
    (define-metafunction name
      store-update : S l StoreVal -> S
      [(store-update () l StoreVal) ((l StoreVal))]
      [(store-update ((l_2 StoreVal_2) (l_3 StoreVal_3) (... ...)) l StoreVal)
       ,(if (equal? (term l) (term l_2))
            ;; The side conditions on E-PutVal should ensure that the
            ;; call to store-update only happens when the lub of the
            ;; old and new values is non-Top.
            (cons (term (l_2 (lub StoreVal StoreVal_2)))
                  (term ((l_3 StoreVal_3) (... ...))))
            (cons (term (l_2 StoreVal_2))
                  (term (store-update ((l_3 StoreVal_3) (... ...)) l StoreVal))))])

    ;; The second condition on the E-GetVal rule.  For any two distinct
    ;; elements in Q, the lub of them is Top.
    (define-metafunction name
      incomp : Q -> Bool
      [(incomp ()) ,#t]
      [(incomp (d)) ,#t]
      [(incomp (d_1 d_2)) ,(equal? (term (lub d_1 d_2)) (term Top))]
      [(incomp (d_1 d_2 d_3 (... ...)))
       ,(and (equal? (term (lub d_1 d_2)) (term Top))
             (term (incomp (d_1 d_3 (... ...))))
             (term (incomp (d_2 d_3 (... ...)))))])

    ;; The third condition on the E-GetVal rule.
    (define-metafunction name
      valid : Q -> Bool
      [(valid Q) ,(not (null? (term Q)))])

    ;; The fourth and fifth premises of the E-GetVal rule.  If there
    ;; exists a d_1 that is a member of Q and is less than or equal to
    ;; d_2, returns that d_1.  Otherwise, returns #f.
    (define-metafunction name
      exists-d : d Q -> d/Bool

      ;; If Q is empty, then there definitely isn't a d_1.
      [(exists-d d_2 ()) #f]

      ;; If the first item in Q is less than d_2, return it.
      [(exists-d d_2 (d_11 d_12 (... ...))) d_11
       (where #t (leq d_11 d_2))]

      ;; Otherwise, check the rest.
      [(exists-d d_2 (d_11 d_12 (... ...))) (exists-d d_2 (d_12 (... ...)))
       (where #f (leq d_11 d_2))])

    ;; subst and subst-vars: capture-avoiding substitution, due to
    ;; redex.racket-lang.org/lam-v.html.

    (define-metafunction name
      subst : x any any -> any
      ;; 1. x_1 bound, so don't continue in lambda body
      [(subst x_1 any_1 (lambda (x_2 (... ...) x_1 x_3 (... ...)) any_2))
       (lambda (x_2 (... ...) x_1 x_3 (... ...)) any_2)
       (side-condition (not (member (term x_1) (term (x_2 (... ...))))))]

      ;; 2. general purpose capture-avoiding case
      [(subst x_1 any_1 (lambda (x_2 (... ...)) any_2))
       (lambda (x_new (... ...))
         (subst x_1 any_1 (subst-vars (x_2 x_new) (... ...) any_2)))
       (where (x_new (... ...)) ,(variables-not-in (term (x_1 any_1 any_2))
                                             (term (x_2 (... ...)))))]

      ;; 3. replace x_1 with e_1
      [(subst x_1 any_1 x_1) any_1]

      ;; 4. x_1 and x_2 are different, so don't replace
      [(subst x_1 any_1 x_2) x_2]

      ;; the last cases cover all other expressions
      [(subst x_1 any_1 (any_2 (... ...)))
       ((subst x_1 any_1 any_2) (... ...))]
      [(subst x_1 any_1 any_2) any_2])

    (define-metafunction name
      subst-vars : (x any) (... ...) any -> any
      [(subst-vars (x_1 any_1) x_1) any_1]
      [(subst-vars (x_1 any_1) (any_2 (... ...)))
       ((subst-vars (x_1 any_1) any_2) (... ...))]
      [(subst-vars (x_1 any_1) any_2) any_2]
      [(subst-vars (x_1 any_1) (x_2 any_2) (... ...) any_3)
       (subst-vars (x_1 any_1)
                   (subst-vars (x_2 any_2) (... ...) any_3))]
      [(subst-vars any) any])))
