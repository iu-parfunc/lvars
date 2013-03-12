#lang racket
(require redex
         "lambdaLVar.rkt")

(define trace-slow
  (lambda (config)
    (traces small-step-slow-rr config)))

(define trace-fast
  (lambda (config)
    (traces small-step-fast-rr config)))

