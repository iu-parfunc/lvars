#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-natpair
  my-lub
  (natural natural)
  (natural Bot)
  (Bot natural))

;; Write a function that takes two pairs (they might be of the formg
;; (natural natural), (natural Bot), (Bot natural), or maybe (Bot
;; Bot)) and returns a pair that is their least upper bound (using max)

;; use car and cadr.  you might have to write a helper

(define my-lub
  (lambda (p1 p2)
    ;; FIXME: define me properly!
    p1))