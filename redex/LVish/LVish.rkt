#lang racket
;; A Redex model of the LVish language.

(provide define-LVish-language)

;; define-LVish-language takes the following arguments:
;;
;;   * a name, e.g. LVish-nat, which becomes the `lang-name` passed to
;;     Redex's `define-language` form.
;;
;;   * a "downset" operation, a Racket-level procedure that takes a
;;     lattice element and returns the (finite) set of all lattice
;;     elements that are below that element.
;;
;;   * a lub operation, a Racket-level procedure that takes two
;;     lattice elements and returns a lattice element.
;;
;;   * some number of lattice elements represented as Redex patterns,
;;     not including top and bottom elements, since we add those
;;     automatically.  (Therefore, if we wanted a lattice consisting
;;     only of Top and Bot, we wouldn't pass any lattice elements to
;;     define-LVish-language.)

(define-syntax-rule (define-LVish-language
                      name
                      downset-op
                      lub-op
                      lattice-elements ...)
  (begin
    (require redex/reduction-semantics)
    (require srfi/1)

    ;; rr is the reduction relation; most of the other operations here
    ;; are exported for testing purposes only.
    (provide rr
             exists-p
             lub
             lub-p
             leq
             extend-H
             contains-all-Q
             first-unhandled-d-in-Q
             store-dom
             lookup-val
             lookup-status
             lookup-state
             update-state
             incomp
             store-dom-diff
             top?
             subst)

    ;; A template for the generated Redex language definition.
    (define-language name

      ;; =============================================================
      ;; LVish syntax

      ;; Configurations on which the reduction relation operates.
      (Config (S e)
              Error)
      
      ;; Expressions.
      (e x
         v
         (e e)
         (get e e)
         (put e e)
         new
         (freeze e)
         (freeze e after e with e)

         ;; An intermediate language form -- this doesn't show up in
         ;; user programs.
         (freeze l after Q with ((callback (lambda (x) e))
                                 (running (e (... ...)))
                                 (handled H)))

         ;; Derived forms; these immediately desugar to application
         ;; and lambda.
         (let ((x e) (x e) (... ...)) e)
         (let par ((x e) (x e) (x e) (... ...)) e))

      ;; Variables.
      (x variable-not-otherwise-mentioned)

      ;; Values.
      (v () ;; unit value
         StoreVal  ;; return value of `freeze ... after ... with ...`
                   ;; (we use StoreVal instead of d here because it
                   ;; will never be Top)

         (StoreVal status) ;; return value of `get` (we use (StoreVal
                           ;; status) instead of p here because it
                           ;; will never be Top-p)

         l  ;; locations (pointers to LVars in the store)
         P  ;; threshold sets
         Q  ;; event sets
         (lambda (x) e))

      ;; Lattice elements, representing the "value" part of the state
      ;; of an LVar (the other part being "status").  We assume Top
      ;; and Bot lattice elements in addition to the user-specified
      ;; set of lattice elements.  A StoreVal can be any element of
      ;; the lattice except Top.
      
      ;; N.B. In the LaTeX grammar, we leave out these next two rules.
      ;; That's because in that grammar, the user-provided lattice
      ;; already comes with Bot and Top, and d is any element of that
      ;; user-provided lattice.  We just use d in the LaTeX grammar in
      ;; every place we use StoreVal here.
      (d StoreVal Top)
      (StoreVal lattice-elements ... Bot)

      ;; Handled element sets.  A handled element set is a finite,
      ;; potentially empty set of lattice elements excluding Top.
      ;; Used to keep track of handled lattice elements in `freeze
      ;; ... after`.
      (H (d (... ...)))

      ;; Stores.  A store is either a finite set of LVars (that is, a
      ;; finite partial mapping from locations l to pairs of StoreVals
      ;; and status flags) or a distinguished value TopS.
      (S (LVar (... ...)) TopS)
      (LVar (l (StoreVal status)))
      (status #t #f)
      (l variable-not-otherwise-mentioned)

      ;; Threshold sets.  A threshold set is the set we pass to a
      ;; `get` expression that specifies a non-empty, pairwise
      ;; incompatible subset of the states of the LVar being queried.

      ;; N.B. Threshold sets are potentially infinite, but we don't
      ;; have a good way to express infinite threshold sets in Redex.
      ;; In the paper, we sometimes define infinite threshold sets
      ;; using predicates.
      (P (p p (... ...)))

      ;; Event sets.  In `freeze l after Q with (lambda (x) e)`, Q is
      ;; the event set.  It's a set of lattice elements on which we
      ;; want (lambda (x) e) to be invoked when l reaches them.  It
      ;; doesn't have to be pairwise incompatible in the way that a
      ;; threshold set does; It's just a set of lattice states.

      ;; N.B. Event sets are potentially infinite, but we don't have a
      ;; good way to express infinite event sets in Redex.
      (Q (d d (... ...)))

      ;; States.
      (p (StoreVal status) Top-p)

      ;; Like P, but potentially empty.  Used in the type of the
      ;; exists-p metafunction.
      (P/null P ())

      ;; Codomains for a couple of metafunctions.
      (Maybe-p p #f)
      (Maybe-d d #f)

      ;; Evaluation contexts.
      (E hole
         (E e)
         (e E)
         (get E e)
         (get e E)
         (put E e)
         (put e E)
         (freeze E)
         (freeze E after e with e)
         (freeze e after E with e)
         (freeze e after e with E)
         (freeze v after v with ((callback v)
                                 (running (e (... ...) E e (... ...)))
                                 (handled H)))

         ;; Special context for desugaring only.
         (let par ((x e) (... ...) (x E) (x e) (... ...)) e)))

    ;; =============================================================
    ;; LVish reduction relation

    (define rr
      (reduction-relation
       name

       ;; Beta-reduction.
       (--> (S (in-hole E ((lambda (x) e) v)))
            (S (in-hole E (subst x v e)))
            "E-Beta")

       ;; Allocation of new LVars.
       (--> (S (in-hole E new))
            ((update-state S l (Bot #f)) (in-hole E l))
            (where l (variable-not-in-store S))
            "E-New")

       ;; Least-upper-bound writes to LVars.

       ;; If an LVar is frozen, putting a value that is less than or
       ;; equal to the current value has no effect...
       (--> (S (in-hole E (put l d_2)))
            ((update-state S l p_2) (in-hole E ()))
            (where p_1 (lookup-state S l))
            (where p_2 (lub-p p_1 (d_2 #f)))
            (where (StoreVal status) p_2)
            "E-Put")

       ;; ...but putting a value that is greater than the current
       ;; value, or has no order with the current value, raises an
       ;; error.
       (--> (S (in-hole E (put l d_2)))
            Error
            (where p_1 (lookup-state S l))
            (where Top-p (lub-p p_1 (d_2 #f)))
            "E-Put-Err")

       ;; Threshold reads from LVars.
       (--> (S (in-hole E (get l P)))
            (S (in-hole E p_2))
            (where p_1 (lookup-state S l))
            (where #t (incomp P))
            (where p_2 (exists-p p_1 P))
            "E-Get")

       ;; Creation of the intermediate language forms that
       ;; E-Spawn-Handler and E-Finalize-Freeze need to operate on.
       (--> (S (in-hole E (freeze l after Q with (lambda (x) e))))
            (S (in-hole E (freeze l after Q with ((callback (lambda (x) e))
                                                  (running ())
                                                  (handled ())))))
            "E-Freeze-Init")

       ;; Launching of handlers.  This rule can fire potentially many
       ;; times for a given `freeze ... after` expression.  It fires
       ;; once for each lattice element d_2 that is:
       ;;
       ;;   * <= the current value d_1 of l.
       ;;   * not a member of the current handled set H.
       ;;   * a member of the event set Q.
       ;;
       ;; For each such d_2, it launches a handler in the `running`
       ;; set and adds d_2 to the `handled` set.
       (--> (S (in-hole E (freeze l after Q with ((callback (lambda (x) e_0))
                                                  (running (e (... ...)))
                                                  (handled H)))))
            (S (in-hole E (freeze l after Q with ((callback (lambda (x) e_0))
                                                  (running ((subst x d_2 e_0) e (... ...)))
                                                  (handled H_2)))))
            (where d_1 (lookup-val S l))
            (where d_2 (first-unhandled-d-in-Q d_1 H Q))
            (where H_2 (extend-H H d_2))
            "E-Spawn-Handler")

       ;; Last step in the evaluation of `freeze ... after`.  When all
       ;; expressions in the `running` set have reached values and all
       ;; lattice elements at or below l's current value have been
       ;; handled, this rule freezes and returns that value.

       ;; N.B.: If we haven't done any writes to an LVar yet (i.e.,
       ;; its value is Bot), then the callback must still run once, to
       ;; add Bot to the `handled` set.  Only then will the premises
       ;; of E-Freeze-Final be satisfied, allowing it to run.
       (--> (S (in-hole E (freeze l after Q with ((callback (lambda (x) e))
                                                  (running (v (... ...)))
                                                  (handled H)))))
            ((freeze-helper S l) (in-hole E d_1))
            (where d_1 (lookup-val S l))
            (where #t (contains-all-Q d_1 H Q))
            "E-Freeze-Final")

       ;; Special case of freeze-after, where there are no handlers to
       ;; run.
       (--> (S (in-hole E (freeze l)))
            ((freeze-helper S l) (in-hole E d_1))
            (where d_1 (lookup-val S l))
            "E-Freeze-Simple")

       ;; ============================================================

       ;; Desugaring of `let`.
       (--> (S (in-hole E (let ((x_1 e_1)) e_2)))
            (S (in-hole E ((lambda (x_1) e_2) e_1)))
            "Desugaring of let")

       ;; Desugaring of `let par`.
       (--> (S (in-hole E (let par ((x_1 e_1) (x_2 e_2)) e_3)))
            (S (in-hole E (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))
            "Desugaring of let par")

       ;; Desugaring of multi-binding `let`
       (--> (S (in-hole E (let ((x_1 e_1)
                                (x_2 e_2)
                                (x_3 e_3) (... ...))
                            e_4)))
            (S (in-hole E (let ((x_1 e_1))
                            (let ((x_2 e_2))
                              (let ((x_3 e_3) (... ...))
                                e_4)))))
            "Desugaring of multi-binding `let`")

       ;; Desugaring of multi-binding `let par`
       (--> (S (in-hole E (let par ((x_1 e_1)
                                    (x_2 e_2)
                                    (x_3 e_3)
                                    (x_4 x_4) (... ...))
                            e_5)))
            (S (in-hole E (let par ((x_1 e_1)
                                    (x (let par ((x_2 e_2)
                                                 (x_3 e_3)
                                                 (x_4 x_4) (... ...))
                                         e_5)))
                            x)))
            (fresh x)
            "Desugaring of multi-binding `let par`")))

    ;; =============================================================
    ;; LVish metafunctions

    ;; Some convenience functions: LVar accessors and constructor.

    (define-metafunction name
      lvloc : LVar -> l
      [(lvloc LVar) ,(first (term LVar))])

    (define-metafunction name
      lvstate : LVar -> p
      [(lvstate LVar) ,(second (term LVar))])
    
    (define-metafunction name
      lvvalue : LVar -> StoreVal
      [(lvvalue LVar) ,(first (second (term LVar)))])

    (define-metafunction name
      lvstatus : LVar -> status
      [(lvstatus LVar) ,(second (second (term LVar)))])

    (define-metafunction name
      build-lv : l StoreVal status -> LVar
      [(build-lv l StoreVal status)
       (l (StoreVal status))])

    ;; Returns a store that is the same as the original store S, but
    ;; with S(l) modified to be frozen.
    (define-metafunction name
      freeze-helper : S l -> S
      [(freeze-helper S l)
       ,(let ([lv (assq (term l) (term S))]
              [update (lambda (lv)
                        (if (equal? (term (lvloc ,lv)) (term l))
                            (term (build-lv (lvloc ,lv) (lvvalue ,lv) #t)) 
                            lv))])
          (if lv
              (term ,(map update (term S)))
              (error "freeze-helper: lookup failed")))])

    ;; Takes a handled set H and returns a new one with d added.
    ;; Assumes that d is not already a member of H.
    (define-metafunction name
      extend-H : H d -> H
      [(extend-H H d) ,(cons (term d) (term H))])

    ;; Checks to see that, for all lattice elements that are less than
    ;; or equal to d and a member of Q, they're a member of H.  In
    ;; other words, (contains-all-Q d H Q) returns true exactly when
    ;; the set (intersection (downset-op d) Q) is a subset of H.
    (define-metafunction name
      contains-all-Q : d H Q -> boolean
      [(contains-all-Q d H Q)
       ,(lset<= equal?
                (lset-intersection equal?
                                   (downset-op (term d))
                                   (term Q))
                (term H))])

    ;; A helper for the E-Spawn-Handler reduction rule.  Takes a
    ;; lattice element d_1, a finite set H of elements, and a finite
    ;; set Q of elements of interest.  returns the first element that
    ;; is <= d_1 in the lattice that is *not* a member of H and *is* a
    ;; member of Q, if such an element exists; returns #f otherwise.
    (define-metafunction name
      first-unhandled-d-in-Q : d H Q -> Maybe-d
      [(first-unhandled-d-in-Q d_1 H Q)
       ,(let ([ls (filter (lambda (x)
                            (and (not (member x (term H)))
                                 (member x (term Q))))
                          (downset-op (term d_1)))])
          (if (null? ls)
              #f
              (term ,(first ls))))])

    (define-metafunction name
      store-dom : S -> (l (... ...))
      [(store-dom ()) ()]
      [(store-dom ((l_1 (StoreVal_1 status_1))
                   (l_2 (StoreVal_2 status_2)) (... ...)))
       ,(cons (term l_1)
              (term (store-dom ((l_2 (StoreVal_2 status_2)) (... ...)))))])

    ;; Return a list of locations in dom(S_1) that are not in dom(S_2).
    (define-metafunction name
      store-dom-diff : S S -> (l (... ...))
      [(store-dom-diff S_1 S_2)
       ,(lset-difference equal?
                         (term (store-dom S_1))
                         (term (store-dom S_2)))])

    (define-metafunction name
      top? : d -> boolean
      [(top? Top) #t]
      [(top? d) #f])

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
      leq : d d -> boolean
      [(leq Bot d_2) #t]
      [(leq d_1 Bot) #f]
      [(leq Top d_2) #f]
      [(leq d_1 Top) #t]

      ;; If d_1 = d_2, then (leq d_1 d_2).
      [(leq d_1 d_2) #t
       (side-condition (equal? (term d_1) (term d_2)))]

      ;; If (lub d_1 d_2) = d_2, then (leq d_1 d_2).
      [(leq d_1 d_2) #t
       (side-condition (equal? (term (lub d_1 d_2)) (term d_2)))]

      ;; If (lub d_1 d_2) = d_1, then (not (leq d_1 d_2)).  (This assumes
      ;; that d_1 != d_2, but we've already covered the case where they're
      ;; equal.)
      [(leq d_1 d_2) #f
       (side-condition (equal? (term (lub d_1 d_2)) (term d_1)))]

      ;; The only case left: (lub d_1 d_2) = d_3, where d_3 is greater
      ;; than both d_1 and d_2.  In this case, (not (leq d_1 d_2)).
      [(leq d_1 d_2) #f])

    ;; The lub operation, but extended to handle status bits:
    (define-metafunction name
      lub-p : p p -> p

      ;; Neither frozen:
      [(lub-p (d_1 #f) (d_2 #f))
       ,(let ([d (term (lub d_1 d_2))])
          (if (equal? d (term Top))
              (term Top-p)
              `(,d #f)))]

      ;; Both frozen:
      [(lub-p (d_1 #t) (d_2 #t))
       ,(if (equal? (term d_1) (term d_2))
            (term (d_1 #t))
            (term Top-p))]

      ;; d_1 unfrozen, d_2 frozen:
      [(lub-p (d_1 #f) (d_2 #t))
       ,(if (term (leq d_1 d_2))
            (term (d_2 #t))
            (term Top-p))]

      ;; d_1 frozen, d_2 unfrozen:
      [(lub-p (d_1 #t) (d_2 #f))
       ,(if (term (leq d_2 d_1))
            (term (d_1 #t))
            (term Top-p))])

    ;; The leq operation, but extended to handle status bits:
    (define-metafunction name
      leq-p : p p -> boolean

      ;; Neither frozen:
      [(leq-p (d_1 #f) (d_2 #f))
       (leq d_1 d_2)]

      ;; Both frozen:
      [(leq-p (d_1 #t) (d_2 #t))
       ,(equal? (term d_1) (term d_2))]

      ;; d_1 unfrozen, d_2 frozen:
      [(leq-p (d_1 #f) (d_2 #t))
       (leq d_1 d_2)]

      ;; d_1 frozen, d_2 unfrozen:
      [(leq-p (d_1 #t) (d_2 #f))
       ,(equal? (term d_1) (term Top))])

    (define-metafunction name
      variable-not-in-store : S -> l
      [(variable-not-in-store S)
       ,(variable-not-in (term S) (term l))])

    (define-metafunction name
      lookup-val : S l -> StoreVal
      [(lookup-val S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvvalue ,lv))
                                 (error "lookup-val: lookup failed")))])

    (define-metafunction name
      lookup-status : S l -> status
      [(lookup-status S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvstatus ,lv))
                                 (error "lookup-status: lookup failed")))])

    (define-metafunction name
      lookup-state : S l -> p
      [(lookup-state S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvstate ,lv))
                                 (error "lookup-state: lookup failed")))])

    ;; Actually handles both updates and extensions.
    (define-metafunction name
      update-state : S l p -> S
      [(update-state () l p) ((l p))]

      [(update-state ((l_2 p_2)
                  (l_3 p_3) (... ...)) l_1 p_1 )
       ,(if (equal? (term l_1) (term l_2))
            ;; The side conditions on E-Put should ensure that the
            ;; call to update-state only happens when the lub of the
            ;; old and new values is non-Top-p.
            (cons (term (l_2 (lub-p p_1 p_2)))
                  (term ((l_3 p_3) (... ...))))
            (cons (term (l_2 p_2))
                  (term (update-state ((l_3 p_3) (... ...)) l_1 p_1))))])

    ;; Used as a premise of the E-Get rule.  Returns #t if, for any
    ;; two distinct elements in P, the lub of them is Top-p, and #f
    ;; otherwise.
    (define-metafunction name
      incomp : P -> boolean
      [(incomp ()) #t]
      [(incomp (p)) #t]
      [(incomp (p_1 p_2)) ,(equal? (term (lub-p p_1 p_2)) (term Top-p))]
      [(incomp (p_1 p_2 p_3 (... ...)))
       ,(and (equal? (term (lub-p p_1 p_2)) (term Top-p))
             (term (incomp (p_1 p_3 (... ...))))
             (term (incomp (p_2 p_3 (... ...)))))])

    ;; Used as a premise of the E-Get rule.  If there exists a p_2
    ;; that is a member of P and is less than or equal to p_1, returns
    ;; that p_2.  Otherwise, returns #f.
    (define-metafunction name
      exists-p : p P/null -> Maybe-p

      ;; If the second argument is null, then there definitely isn't a p_2.
      [(exists-p p_1 ()) #f]

      ;; If the first item in P is less than p_1, return it.
      [(exists-p p_1 (p_21 p_22 (... ...))) p_21
       (where #t (leq-p p_21 p_1))]

      ;; Otherwise, check the rest.
      [(exists-p p_1 (p_21 p_22 (... ...))) (exists-p p_1 (p_22 (... ...)))
       (where #f (leq-p p_21 p_1))])

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
