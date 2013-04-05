#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-natpair-ivars
  my-lub
  (natural natural)
  (natural Bot)
  (Bot natural))

;; Write a function that takes two pairs (they might be of the form
;; (natural natural), (natural Bot), (Bot natural), or maybe (Bot
;; Bot)) and returns a pair that is their least upper bound.

;; BUT because they're IVars, we have to combine them in a different
;; way.  You can only safely combine two pairs if one of them has only
;; the car filled in, and the other has only the cadr filled in.

;; Otherwise you have to return 'Top.

;; use car and cadr.  you might have to write a helper

(define my-lub
  (lambda (p1 p2)
    ;; FIXME: define me!
    p1))