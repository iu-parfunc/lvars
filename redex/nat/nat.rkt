;; A Redex model of the lambdaLVar language.  Mentions of "the TR" in
;; this document refer to
;; http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR702

#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-nat natural max)
