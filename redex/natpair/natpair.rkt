#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-natpair
  ;; TODO: lindsey needs to make sure this works.
  ((natural natural)
   (natural Bot)
   (Bot natural))
  my-lub
  )

;; Write a function that takes two pairs (they might be of the form
;; (natural natural), (natural Bot), (Bot natural), or maybe (Bot
;; Bot)) and returns a pair that is their least upper bound (using max)

;; use car and cadr.  you might have to write a helper

(define my-lub
  (lambda (p1 p2)
    ;; FIXME: define me!
    ... ))