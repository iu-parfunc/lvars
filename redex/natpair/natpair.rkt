#lang racket
(require redex/reduction-semantics)
(require "../lambdaLVar.rkt")

(define-lambdaLVar-language lambdaLVar-natpair
  my-lub
  (natural natural)
  (natural Bot)
  (Bot natural))

;; Write a function that takes two pairs (they might be of the form
;; (natural natural), (natural Bot), (Bot natural), or maybe (Bot
;; Bot)) and returns a pair that is their least upper bound (using max)

;; use car and cadr.  you might have to write a helper

(define my-lub
  (lambda (p1 p2)
    ;; FIXME: define me properly!
    (let* ([x_1 (car p1)]
          [x_2 (cadr p1)]
          [y_1 (car p2)]
          [y_2 (cadr p2)]
          [max_1 (mymax x_1 y_1)]
          [max_2 (mymax x_2 y_2)])
      (list max_1 max_2))))

(define mymax 
  (lambda (z_1 z_2)
    (cond 
      [(and (number? z_1) (number? z_2))
       (max z_1 z_2)]
      [(number? z_1)
       z_1]
      [(number? z_2)
       z_2]
      [else (error "You didn't pass in a valid argument")])))
          
    