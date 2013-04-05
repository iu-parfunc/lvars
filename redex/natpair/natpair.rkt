#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-natpair
  ;; FIXME: lattice values go here
  ((natural natural)
   (natural Bot)
   (Bot natural))
  ;; FIXME: lub operation goes here
  max
  )

;; Write a function that takes two pairs (they might be of the form
;; (natural natural), (natural Bot), (Bot natural), or maybe (Bot
;; Bot)) and returns a pair that is their least upper bound.

;; use car and cadr.  you might have to write a helper

(define my-lub
  (lambda (p1 p2)
    ... ))