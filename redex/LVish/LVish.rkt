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
;;   * some number of lattice values represented as Redex patterns,
;;     not including top and bottom elements, since we add those
;;     automatically.  (Therefore, if one wanted a lattice consisting
;;     only of Top and Bot, they wouldn't pass any lattice values to
;;     define-LVish-language.)

(define-syntax-rule (define-LVish-language
                      name
                      downset-op
                      lub-op
                      lattice-values ...)
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
             extend-Df
             contains-all-leq
             first-unhandled-d
             store-dom
             lookup-val
             lookup-status
             lookup-p
             update-val
             incomp
             store-dom-diff
             store-top?
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
         (freeze e after e)

         ;; An intermediate language form -- this doesn't show up in
         ;; user programs.
         (freeze e after ((callback (lambda (x) e))
                          (running (e (... ...)))
                          (handled Df)))

         ;; Derived forms; these immediately desugar to application
         ;; and lambda.
         (let ((x e)) e)
         (let par ((x e) (x e)) e))

      ;; Variables.
      (x variable-not-otherwise-mentioned)

      ;; Values.
      (v ()  ;; unit value
         d   ;; return value of `freeze ... after`
         p   ;; pair of d and status; return value of `get`
         l   ;; locations (pointers to LVars in the store)
         Q   ;; threshold sets
         (lambda (x) e))

      ;; Lattice elements, representing the state of an LVar.  We
      ;; assume Top and Bot lattice elements in addition to the
      ;; user-specified set of lattice values.  A StoreVal can be any
      ;; element of the lattice except Top.  That is, here we actually
      ;; rule out a StoreVal being Top, whereas in the LaTeX grammar
      ;; we don't.
      (d Top StoreVal)
      (StoreVal lattice-values ... Bot)

      ;; Potentially empty set of lattice values, excluding Top.  Used
      ;; to keep track of handled lattice values in `freeze ... after`.
      (Df (d (... ...)))

      ;; Stores.  A store is either a set of LVars (that is, a finite
      ;; partial mapping from locations l to pairs of StoreVals and
      ;; status flags) or a distinguished value TopS.
      (S (LVar (... ...)) TopS)
      (LVar (l (StoreVal status)))
      (status #t #f)
      (l variable-not-otherwise-mentioned)

      ;; Threshold sets.  A threshold set is the set we pass to a
      ;; `get` expression that specifies a non-empty, pairwise
      ;; incompatible subset of the state space of the location being
      ;; queried.

      ;; Incidentally, under this grammar, ((Top status)) and ((Bot
      ;; status)) are threshold sets. The latter might make sense, but
      ;; the former is nonsensical -- a program that had Top as a
      ;; threshold would block forever.  Nevertheless, the grammar
      ;; admits it.
      (Q (p p (... ...)))
      (p TopP (d status))

      ;; Like Q, but potentially empty.  Used in the type of the
      ;; exists-p metafunction.
      (Q/null Q ())

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
         (freeze E after e)
         (freeze v after ((callback (lambda (x) e))
                          (running (e (... ...) E e (... ...)))
                          (handled Df)))))

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
            ((update-val S l Bot) (in-hole E l))
            (where l (variable-not-in-store S))
            "E-New")

       ;; Least-upper-bound writes to unfrozen LVars.
       (--> (S (in-hole E (put l d_2)))
            ((update-val S l d_2) (in-hole E ()))
            (where d_1 (lookup-val S l))
            (where #f (lookup-status S l))
            (where #f (top? (lub d_1 d_2)))
            "E-Put")

       ;; Least-upper-bound writes to frozen LVars.  If an LVar is
       ;; frozen, putting a value that is less than or equal to the
       ;; current value has no effect...
       (--> (S (in-hole E (put l d_2)))
            (S (in-hole E ()))
            (where d_1 (lookup-val S l))
            (where #t (lookup-status S l))
            (where #t (leq d_2 d_1))
            "E-Put-Frozen")

       ;; ...but putting a value that is greater than the current
       ;; value, or has no order with the current value, raises an
       ;; error.
       (--> (S (in-hole E (put l d_2)))
            Error
            (where #t (lookup-status S l))
            (where d_1 (lookup-val S l))
            (where #f (leq d_2 d_1))
            "E-Put-Frozen-Err")

       ;; Threshold reads from LVars.
       (--> (S (in-hole E (get l Q)))
            (S (in-hole E p_2))
            (where p_1 (lookup-p S l))
            (where #t (incomp Q))
            (where p_2 (exists-p p_1 Q))
            "E-Get")

       ;; Desugaring of `let`.
       (--> (S (in-hole E (let ((x_1 e_1)) e_2)))
            (S (in-hole E ((lambda (x_1) e_2) e_1)))
            "Desugaring of let")

       ;; Desugaring of `let par`.
       (--> (S (in-hole E (let par ((x_1 e_1) (x_2 e_2)) e_3)))
            (S (in-hole E (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))
            "Desugaring of let par")

       ;; Error propagation due to conflicting writes.
       (--> (S (in-hole E (put l d_2)))
            Error
            (where d_1 (lookup-val S l))
            (where #t (top? (lub d_1 d_2)))
            "E-Put-Err")

       ;; Creation of the intermediate language forms that
       ;; E-Spawn-Handler and E-Finalize-Freeze need to operate on.
       (--> (S (in-hole E (freeze l after (lambda (x) e))))
            (S (in-hole E (freeze l after ((callback (lambda (x) e))
                                           (running ())
                                           (handled ())))))
            "E-Freeze-Init")


       ;; Launching of handlers.  This rule can fire potentially many
       ;; times for a given `freeze ... after` expression.  It fires
       ;; once for each lattice element d_2 that is <= the current
       ;; state d_1 of l, so long as that element is not already a
       ;; member of Df.  For each such d_2, it launches a handler in
       ;; the `running` set and adds d_2 to the `handled` set.
       (--> (S (in-hole E (freeze l after ((callback (lambda (x) e_1))
                                           (running (e_2 (... ...)))
                                           (handled Df)))))
            (S (in-hole E (freeze l after ((callback (lambda (x) e_1))
                                           (running ((subst x d_2 e_1) e_2 (... ...)))
                                           (handled Df_2)))))
            (where d_1 (lookup-val S l))
            (where d_2 (first-unhandled-d d_1 Df))
            (where Df_2 (extend-Df Df d_2))
            "E-Spawn-Handler")

       ;; Last step in the evaluation of `freeze ... after`.  When all
       ;; expressions in the `running` set have reached values and all
       ;; lattice elements at or below l's current state have been
       ;; handled, this rule freezes and returns that state.

       ;; N.B.: If we haven't done any writes to an LVar yet (i.e.,
       ;; its state is Bot), then the callback must still run once, to
       ;; add Bot to the `handled` set.  Only then will the premises
       ;; of E-Finalize-Freeze be satisfied, allowing it to run.
       (--> (S (in-hole E (freeze l after ((callback (lambda (x) e))
                                           (running (v (... ...)))
                                           (handled Df)))))
            ((freeze-helper S l) (in-hole E d_1))
            (where d_1 (lookup-val S l))
            (where #t (contains-all-leq d_1 Df))
            "E-Finalize-Freeze")

       ;; Special case of freeze-after, where there are no handlers to
       ;; run.
       (--> (S_1 (in-hole E (freeze l after ())))
            (S_2 (in-hole E d))
            ;; N.B.: Redex gotcha: the order of these two `where`
            ;; clauses matters.  :(
            (where S_2 (freeze-helper S_1 l))
            (where d (lookup-val S_2 l))
            "E-Freeze")))

    ;; =============================================================
    ;; LVish metafunctions

    ;; Some convenience functions: LVar accessors and constructor.

    (define-metafunction name
      lvloc : LVar -> l
      [(lvloc LVar) ,(first (term LVar))])

    (define-metafunction name
      lvp : LVar -> p
      [(lvp LVar) ,(second (term LVar))])
    
    (define-metafunction name
      lvstate : LVar -> StoreVal
      [(lvstate LVar) ,(first (second (term LVar)))])

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
                            (term (build-lv (lvloc ,lv) (lvstate ,lv) #t)) 
                            lv))])
          (if lv
              (term ,(map update (term S)))
              (error "freeze-helper: lookup failed")))])

    ;; Returns a Df set with d added.  Assumes that d is not already a
    ;; member of Df.
    (define-metafunction name
      extend-Df : Df d -> Df
      [(extend-Df Df d) ,(cons (term d) (term Df))])

    ;; Checks to see that, for all lattice elements that are less than
    ;; or equal to d, they're a member of Df.  In other words, the set
    ;; (downset-op d) is a subset of Df.
    (define-metafunction name
      contains-all-leq : d Df -> boolean
      [(contains-all-leq d Df)
       ,(lset<= equal?
                (downset-op (term d))
                (term Df))])

    ;; A helper for the E-Spawn-Handler reduction rule.  Takes a
    ;; lattice element d_1 and a finite set Df of elements, and
    ;; returns the first element that is <= d_1 in the lattice that is
    ;; *not* a member of Df, if such an element exists; returns #f
    ;; otherwise.
    (define-metafunction name
      first-unhandled-d : d Df -> Maybe-d
      [(first-unhandled-d d_1 Df)
       ,(let ([ls (filter (lambda (x)
                            (not (member x (term Df))))
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

    ;; The greatest element of the store lattice is any store in which
    ;; some location is bound to Top.
    (define-metafunction name
      store-top? : S -> boolean
      [(store-top? TopS) #t]
      [(store-top? S) #f])

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
              (term TopP)
              `(,d #f)))]

      ;; Both frozen:
      [(lub-p (d_1 #t) (d_2 #t))
       ,(if (equal? (term d_1) (term d_2))
            (term (d_1 #t))
            (term TopP))]

      ;; d_1 unfrozen, d_2 frozen:
      [(lub-p (d_1 #f) (d_2 #t))
       ,(if (term (leq d_1 d_2))
            (term (d_2 #t))
            (term TopP))]

      ;; d_1 frozen, d_2 unfrozen:
      [(lub-p (d_1 #t) (d_2 #f))
       ,(if (term (leq d_2 d_1))
            (term (d_1 #t))
            (term TopP))])

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
                                 (term (lvstate ,lv))
                                 (error "lookup-val: lookup failed")))])

    (define-metafunction name
      lookup-status : S l -> status
      [(lookup-status S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvstatus ,lv))
                                 (error "lookup-status: lookup failed")))])

    (define-metafunction name
      lookup-p : S l -> p
      [(lookup-p S l) ,(let ([lv (assq (term l) (term S))])
                             (if lv
                                 (term (lvp ,lv))
                                 (error "lookup-p lookup failed")))])

    ;; Actually handles both updates and extensions.  Assumes that if
    ;; l is in dom(S), that it is unfrozen.
    (define-metafunction name
      update-val : S l StoreVal -> S

      ;; If adding a binding that wasn't in the store before (an
      ;; extension), it is unfrozen.
      [(update-val () l StoreVal) ((l (StoreVal #f)))]

      [(update-val ((l_2 (StoreVal_2 status_2))
                      (l_3 (StoreVal_3 status_3)) (... ...)) l StoreVal)
       ,(if (equal? (term l) (term l_2))
            ;; The side conditions on E-Put should ensure that the
            ;; call to update-val only happens when the lub of the
            ;; old and new values is non-Top.
            (cons (term (l_2 ((lub StoreVal StoreVal_2) status_2)))
                  (term ((l_3 (StoreVal_3 status_3)) (... ...))))
            (cons (term (l_2 (StoreVal_2 status_2)))
                  (term (update-val ((l_3 (StoreVal_3 status_3)) (... ...)) l StoreVal))))])

    ;; Used as a premise of the E-Get rule.  Returns #t if, for any
    ;; two distinct elements in Q, the lub of them is Top, and #f
    ;; otherwise.
    (define-metafunction name
      incomp : Q -> boolean
      [(incomp ()) #t]
      [(incomp (p)) #t]
      [(incomp (p_1 p_2)) ,(equal? (term (lub-p p_1 p_2)) (term TopP))]
      [(incomp (p_1 p_2 p_3 (... ...)))
       ,(and (equal? (term (lub-p p_1 p_2)) (term TopP))
             (term (incomp (p_1 p_3 (... ...))))
             (term (incomp (p_2 p_3 (... ...)))))])

    ;; Used as a premise of the E-Get rule.  If there exists a p_2
    ;; that is a member of Q and is less than or equal to p_1, returns
    ;; that p_2.  Otherwise, returns #f.
    (define-metafunction name
      exists-p : p Q/null -> Maybe-p

      ;; If the second argument is null, then there definitely isn't a p_2.
      [(exists-p p_1 ()) #f]

      ;; If the first item in Q is less than p_1, return it.
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
