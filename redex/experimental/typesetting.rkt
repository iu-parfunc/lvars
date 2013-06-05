#lang racket

;; Some examples of ways to make the typesetting features work for
;; macro-generated Redex, courtesy of Robby Findler.

(require redex/reduction-semantics
         redex/pict)
(define-syntax-rule 
  (mk-L L a ...)
  (define-language L
    (e (λ (x) e)
       (e e)
       a ...
       x)
    (x variable-not-otherwise-mentioned)))

(mk-L L + - *)
(with-handlers ((exn:fail? exn-message))
  (render-language L))

(define-syntax-rule 
  (mk-L2 L a ...)
  (define-language L
    (e (λ (x) e)
       (e e)
       prims
       x)
    (prims a ...)
    (x variable-not-otherwise-mentioned)))

(mk-L2 L2 + - *)

;; Here the "nts" attribute means "nonterminals", and you pass it a
;; list of only those nonterminals that you want to typeset.  So, by
;; avoiding "prims", you can avoid the problem.  This probably won't
;; work as a fix for us.
(render-language L2 #:nts '(e x))

;; Rather, we'll need to do something more like this, where we
;; actually munge the source locations:
(define-syntax (mk-L3 stx)
  (syntax-case stx ()
    [(_ L a ...)
     (let ()
       (define template
         #'(define-language L
             (e (λ (x) e)
                (e e)
                HERE
                x)
             (x variable-not-otherwise-mentioned)))
       (car
        (let loop ([stx template])
          (syntax-case stx (HERE)
            [HERE 
             (let loop ([as (syntax->list #'(a ...))]
                        [pos (syntax-position stx)]
                        [col (syntax-column stx)])
               (cond
                 [(null? as) '()]
                 [else 
                  (define a (car as))
                  (define span (string-length (symbol->string (syntax-e a))))
                  (cons
                   (datum->syntax a 
                                  (syntax-e a)
                                  (vector (syntax-source stx)
                                          (syntax-line stx)
                                          col
                                          pos
                                          span)
                                  a)
                   (loop (cdr as)
                         (+ pos span 1)
                         (+ col span 1)))]))]
            [(a ...)
             (list
              (datum->syntax
               stx
               (apply append (map loop (syntax->list #'(a ...))))
               stx
               stx))]
            [a
             (list stx)]))))]))

(mk-L3 L3 + - *)
(render-language L3)