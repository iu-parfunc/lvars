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
