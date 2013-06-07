#lang racket
;; A Redex model of the LVish language.

(provide define-LVish-language)

;; define-LVish-language takes a language name, a lub operation, and
;; some number of lattice values, not including top and bottom
;; elements, since we add those automatically.  (Therefore, if one
;; wanted a lattice consisting only of Top and Bot, they wouldn't pass
;; any lattice values to define-LVish-language.)

(define-syntax-rule (define-LVish-language name lub-op lattice-values ...)
  (begin
    (require redex/reduction-semantics)
    (require srfi/1)

    (provide rr
             exists-d
             lub
             leq
             store-dom
             lookup-val
             lookup-frozenness
             update-val
             incomp
             store-dom-diff
             store-top?
             top?
             subst)
    
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
         (freeze e after e with e)

         ;; These immediately desugar to application and lambda.
         (let ((x e)) e)
         (let par ((x e) (x e)) e))

      ;; Values.
      (v () ;; unit value
         d
         l
         Q
         (lambda (x) e))

      ;; Threshold set literals.  A threshold set is the set we pass to a
      ;; `get` expression that specifies a non-empty, pairwise
      ;; incompatible subset of the state space of the location being
      ;; queried.

      ;; Incidentally, under this grammar, (Top) and (Bot) are threshold
      ;; sets. (Bot) makes sense, but (Top) is nonsensical -- a program
      ;; that passed (Top) as a threshold would block forever.
      ;; Nevertheless, the grammar admits it.
      (Q (d d (... ...)))

      ;; Stores.  A store is either a set of LVars (that is, a finite
      ;; partial mapping from locations l to pairs of StoreVals and
      ;; frozenness flags) or a distinguished value TopS.
      (S (LVar (... ...)) TopS)
      (LVar (l (StoreVal frozenness)))
      (frozenness #t #f)

      ;; Lattice elements, representing the state of an LVar.  We
      ;; assume Top and Bot lattice elements in addition to the
      ;; user-specified set d of lattice values.  A StoreVal can be
      ;; any element of the lattice except Top.  That is, here we
      ;; actually rule out a StoreVal being Top, whereas in the LaTeX
      ;; grammar we don't.
      (d Top StoreVal)
      (StoreVal lattice-values ... Bot)

      ;; Ranges of a couple of metafunctions.
      (Bool #t #f)
      (d/Bool d Bool)
      (Q/null Q ())

      (x variable-not-otherwise-mentioned)
      (l variable-not-otherwise-mentioned)

      ;; Evaluation contexts.
      (E hole
         (E e)
         (e E)
         (get E e)
         (get e E)
         (put E e)
         (put e E)
         (freeze E after v with e)
         (freeze v after v with E)
         (freeze v after E with v)
         (freeze v after (v
                          (e (... ...) E e (... ...)))
                 with v)))

    (define rr
      (reduction-relation
       name

       (--> (S (in-hole E ((lambda (x) e) v)))
            (S (in-hole E (subst x v e)))
            "E-Beta")

       (--> (S (in-hole E new))
            ((update-val S l Bot) (in-hole E l))
            (where l (variable-not-in-store S))
            "E-New")

       (--> (S (in-hole E (put l d_2)))
            ((update-val S l d_2) (in-hole E ()))
            (where d_1 (lookup-val S l))
            (where #f (lookup-frozenness S l))
            (where #f (top? (lub d_1 d_2)))
            "E-Put")

       ;; If an LVar is frozen, putting a value that is less than or
       ;; equal to the current value has no effect...
       (--> (S (in-hole E (put l d_2)))
            (S (in-hole E ()))
            (where d_1 (lookup-val S l))
            (where #t (lookup-frozenness S l))
            (where #t (leq d_2 d_1))
            "E-Put-Frozen")

       ;; ...but putting a value that is greater than the current
       ;; value, or has no order with the current value, raises an
       ;; error.
       (--> (S (in-hole E (put l d_2)))
            Error
            (where #t (lookup-frozenness S l))
            (where d_1 (lookup-val S l))
            (where #f (leq d_2 d_1))
            "E-Put-Frozen-Err")

       (--> (S (in-hole E (get l Q)))
            (S (in-hole E d_2))
            (where d_1 (lookup-val S l))
            (where #t (incomp Q))
            (where d_2 (exists-d d_1 Q))
            "E-Get")

       (--> (S (in-hole E (let ((x_1 e_1)) e_2)))
            (S (in-hole E ((lambda (x_1) e_2) e_1)))
            "Desugaring of let")

       (--> (S (in-hole E (let par ((x_1 e_1) (x_2 e_2)) e_3)))
            (S (in-hole E (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))
            "Desugaring of let par")

       ;; Propagates errors due to conflicting writes.
       (--> (S (in-hole E (put l d_2)))
            Error
            (where d_1 (lookup-val S l))
            (where #t (top? (lub d_1 d_2)))
            "E-Put-Err")

       ;; OK, so what's the deal with freeze-after?  what does it do?

       ;; (freeze e_1 after e_2 with e_3) behaves as follows:

       ;; e_1 evals to a location, so we don't have to worry about it
       ;; at all.  We can write all our rules with `l` there.

       ;; e_2 is a piece of code that will run every time l is written
       ;; to.  For instance, if l represents a set of nodes in a
       ;; graph, then for each node put into the set we might want
       ;; this code to run:

       ;; foreach neighbor in (neighbors node):
       ;;   put l { neighbor }

       ;; So, in all, e_2 is:

       ;; foreach node in l:
       ;;   foreach neighbor in (neighbors node):
       ;;     put l { neighbor }

       ;; But you can't actually write something like "foreach node in
       ;; l" directly in LVish because that would involve peeking at
       ;; the contents of l.  So there has to be something built in to
       ;; freeze-after that allows you to peek at LVar contents.

       ;; Of course, we won't have a pointer to l; that's what e_1
       ;; evaluates to.  So we'll need a bound variable to represent
       ;; it, and actually, e_2 is:

       ;; (lambda (x)
       ;;   foreach node in x:
       ;;     foreach neighbor in (neighbors node):
       ;;       put x { neighbor })

       ;; where x is the exact contents of l.  In fact, e_2 should
       ;; always be of this form, and running e_2 means applying e_2
       ;; to l, which means, of course, substituting those exact
       ;; contents into the body of e_2 in place of x.

       ;; Something like this:

       ;; (--> (S (in-hole E (freeze l after (lambda (x) e) with v)))
       ;;      (S (in-hole E (freeze l after (subst x d e) with v)))
       ;;      (where d (lookup-val S l))
       ;;      "E-Freeze-Beta")

       ;; But this isn't right, because it's only triggered once.  We
       ;; should make the (lambda (x) e) stick around to be triggered
       ;; again in case of future writes.  How about a couple of
       ;; lists?


       (--> (S (in-hole E (freeze l after (lambda (x) e) with v)))
            (S (in-hole E (freeze l after ((lambda (x) e) ((subst x d e))) with v)))
            (where d (lookup-val S l))
            "E-Freeze-Init")

       ;; Eval contexts should take care of reducing that, but how do
       ;; we make sure more handlers get added every time a write to l
       ;; occurs?

       ;; It seems like there will have to be some information hanging
       ;; off of l in the store to make that occur.

       ;; In any case, if only one handler is ever added, we'll
       ;; eventually get to:

       (--> (S (in-hole E (freeze l after ((lambda (x) e) (v_1)) with v_2)))
            (S (in-hole E (freeze l after () with v_2)))
            "E-Freeze-Done")

       ;; Which should trigger E-Freeze, finally.  (v_1 gets thrown
       ;; away -- we don't care about the return values of handlers.)

       ;; TODO: keep working on this.


       ;; -------------------------------------


       ;; Special case of freeze-after, where there are no handlers to
       ;; run.

       ;; At this point, since v is a value, it's already done its
       ;; write to the store, S_1.  So, freeze-helper's job is merely
       ;; to freeze the appropriate location.
       (--> (S_1 (in-hole E (freeze l after () with v)))
            (S_2 (in-hole E d))
            ;; N.B.: Redex gotcha: the order of these two `where`
            ;; clauses matters.  :(
            (where S_2 (freeze-helper S_1 l))
            (where d (lookup-val S_2 l))
            "E-Freeze")))

    ;; Some convenience functions: LVar accessors and constructor.

    (define-metafunction name
      lvloc : LVar -> l
      [(lvloc LVar) ,(first (term LVar))])
    
    (define-metafunction name
      lvstate : LVar -> StoreVal
      [(lvstate LVar) ,(first (second (term LVar)))])

    (define-metafunction name
      lvfrozenness : LVar -> frozenness
      [(lvfrozenness LVar) ,(second (second (term LVar)))])

    (define-metafunction name
      build-lv : l StoreVal frozenness -> LVar
      [(build-lv l StoreVal frozenness)
       (l (StoreVal frozenness))])

    ;; Returns a store that is the same as the original store S, but
    ;; with S(l) modified to be frozen.
    (define-metafunction name
      freeze-helper : S l -> S
      [(freeze-helper S l)
       ,(let ([lv (assq (term l) (term S))]
              [update (lambda (lv)
                        (if (equal? (term (lvloc ,lv)) (term l))
                            (term (build-lv (lvloc ,lv) (lvstate ,lv) #t)) 
                            lv))])
          (if lv
              (term ,(map update (term S)))
              (error "freeze-helper: lookup failed")))])

    (define-metafunction name
      store-dom : S -> (l (... ...))
      [(store-dom ()) ()]
      [(store-dom ((l_1 (StoreVal_1 frozenness_1))
                   (l_2 (StoreVal_2 frozenness_2)) (... ...)))
       ,(cons (term l_1)
              (term (store-dom ((l_2 (StoreVal_2 frozenness_2)) (... ...)))))])

    ;; Return a list of locations in dom(S_1) that are not in dom(S_2).
    (define-metafunction name
      store-dom-diff : S S -> (l (... ...))
      [(store-dom-diff S_1 S_2)
       ,(lset-difference equal?
                         (term (store-dom S_1))
                         (term (store-dom S_2)))])

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
    ;; Intended to be extended by a user-provided operation.
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


    (define-metafunction name
      variable-not-in-store : S -> l
      [(variable-not-in-store S)
       ,(variable-not-in (term S) (term l))])

    (define-metafunction name
      lookup-val : S l -> StoreVal
      [(lookup-val S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvstate ,lv))
                                 (error "lookup-val: lookup failed")))])

    (define-metafunction name
      lookup-frozenness : S l -> frozenness
      [(lookup-frozenness S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvfrozenness ,lv))
                                 (error "lookup-frozenness: lookup failed")))])

    ;; Actually handles both updates and extensions.  Assumes that if
    ;; l is in dom(S), that it is unfrozen.
    (define-metafunction name
      update-val : S l StoreVal -> S

      ;; If adding a binding that wasn't in the store before (an
      ;; extension), it is unfrozen.
      [(update-val () l StoreVal) ((l (StoreVal #f)))]

      [(update-val ((l_2 (StoreVal_2 frozenness_2))
                      (l_3 (StoreVal_3 frozenness_3)) (... ...)) l StoreVal)
       ,(if (equal? (term l) (term l_2))
            ;; The side conditions on E-Put should ensure that the
            ;; call to update-val only happens when the lub of the
            ;; old and new values is non-Top.
            (cons (term (l_2 ((lub StoreVal StoreVal_2) frozenness_2)))
                  (term ((l_3 (StoreVal_3 frozenness_3)) (... ...))))
            (cons (term (l_2 (StoreVal_2 frozenness_2)))
                  (term (update-val ((l_3 (StoreVal_3 frozenness_3)) (... ...)) l StoreVal))))])

    ;; The second condition on the E-Get rule.  For any two distinct
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

    ;; The third and fourth premises of the E-Get rule.  If there
    ;; exists a d_1 that is a member of Q and is less than or equal to
    ;; d_2, returns that d_1.  Otherwise, returns #f.
    (define-metafunction name
      exists-d : d Q/null -> d/Bool

      ;; If the second argument is null, then there definitely isn't a d_1.
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
