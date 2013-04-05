#lang racket

(provide cfgs-equal-modulo-perms?
         stores-equal-modulo-perms?)
(require srfi/1)

;; Takes two (S e) configurations and returns #t if they're equal
;; modulo permutations of store bindings.
(define cfgs-equal-modulo-perms?
  (lambda (cfg1 cfg2)
    (and (stores-equal-modulo-perms? (car cfg1) (car cfg2))
         (equal? (cdr cfg1) (cdr cfg2)))))

;; Takes two stores and returns #t if they're equal modulo
;; permutations.
(define stores-equal-modulo-perms?
  (lambda (s1 s2)
    (lset= equal? s1 s2)))
