#lang racket
(require redex
         "lambdaLVar.rkt")

(define trace-base
  (lambda (config)
    (traces small-step-base-rr config)))

(define trace-fast
  (lambda (config)
    (traces small-step-fast-rr config)))

