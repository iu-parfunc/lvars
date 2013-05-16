#lang racket
;; A Redex model of the LVish language.

(provide define-LVish-language)

(define-syntax-rule (define-LVish-language name lub-op lattice-values ...)
  (begin
    (require redex/reduction-semantics)
    (require srfi/1)

    (provide rr
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

         ;; New additions in LVish.
         (consume e)
         (let handlers x be (bind (δ) e) with e in e)

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

      ;; Stores.  A store is either a finite partial mapping from
      ;; locations to domain values (excluding Top), or it is the
      ;; distinguished element TopS.
      (S ((l StoreVal) (... ...)) TopS)

      ;; Domains contain elements d to which locations can be bound.  We
      ;; assume a domain of Top and Bot (which is intended to be
      ;; extended).  A StoreVal can be any element of the domain except
      ;; Top (see Definition 1 in the TR).
      (d Top StoreVal)
      (StoreVal lattice-values ... Bot)

      ;; Ranges of a couple of metafunctions.
      (d/lookupfailed d lookupfailed)
      (Bool #t #f)
      (d/Bool d Bool)

      (x variable-not-otherwise-mentioned)
      (l variable-not-otherwise-mentioned)

      ;; Evaluation contexts.
      (E hole
         (E e)
         (v E)
         (get E e)
         (get v E)
         (put E e)
         (put v E)
         (consume E)
         (let handlers x be (bind (δ) e) with E in e)
         (let handlers x be (bind (δ) e) with v in E)))

    (define rr
      (reduction-relation
       name

       (--> (S (in-hole E ((lambda (x) e) v)))
            (S (in-hole E (subst x v e)))
            "E-Beta")

       (--> (S (in-hole E new))
            ((store-update S l Bot) (in-hole E l))
            (where l (variable-not-in-store S))
            "E-New")

       (--> (S (in-hole E (put l (d_2))))
            ((store-update S l d_2) (in-hole E ()))
            (where d_1 (store-lookup S l))
            (where #f (top? (lub d_1 d_2)))
            "E-PutVal")

       (--> (S (in-hole E (get l Q)))
            (S (in-hole E (d_1)))
            (where d_2 (store-lookup S l))
            (where #t (incomp Q))
            (where #t (valid Q))
            (where d_1 (exists-d d_2 Q))
            "E-GetVal")

       (--> (S (in-hole E (let ((x_1 e_1)) e_2)))
            (S (in-hole E ((lambda (x_1) e_2) e_1)))
            "Desugaring of let")

       (--> (S (in-hole E (let par ((x_1 e_1) (x_2 e_2)) e_3)))
            (S (in-hole E (((lambda (x_1) (lambda (x_2) e_3)) e_1) e_2)))
            "Desugaring of let par")

       ;; TODO: This needs more work.
       (--> (S (in-hole E (S (let handlers x be (bind (δ) e_1) with e_2 in e_3))))
            ;; Make sure that e_3 happens last.
            (S (in-hole E (S ((lambda (x_2) e_3) e_2))))
            (where x_3 (variable-not-in e_3))
            "Desugaring of let handlers")

       (--> (S (in-hole E (convert Q)))
            (S (in-hole E (conv Q)))
            "E-ConvertVal")

       ;; Propagates errors due to conflicting writes.
       (--> (S (in-hole E (put l (d_2))))
            Error
            (where d_1 (store-lookup S l))
            (where #t (top? (lub d_1 d_2)))
            "E-PutValErr")

       ;; TODO: handle `let handlers`.
       ;; TODO: handle `consume`.

))

    (define-metafunction name
      store-dom : S -> (l (... ...))
      [(store-dom ()) ()]
      [(store-dom ((l_1 d_1) (l_2 d_2) (... ...)))
       ,(cons (term l_1) (term (store-dom ((l_2 d_2) (... ...)))))])

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
      store-lookup : S l -> d/lookupfailed
      [(store-lookup S l) ,(let ([v (assq (term l) (term S))])
                             (if v
                                 (term ,(second v))
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

    (define-metafunction name
      conv : Q -> Q
      ;; Let's just have it be identity for now ...
      [(conv Q) Q])

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
