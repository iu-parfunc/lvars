#lang racket
(require redex/reduction-semantics)
(require "LVish.rkt")

(define-LVish-language LVish-natpair-ivars
  my-lub
  (natural natural)
  (natural Bot)
  (Bot natural))

;; A function that takes two pairs (they might be of the form (natural
;; natural), (natural Bot), or (Bot natural)) and returns a pair that
;; is their least upper bound.

;; Because they're IVars, we can only safely combine two pairs if one
;; of them has only the car filled in, and the other has only the cadr
;; filled in.

;; assumes that a1 and a2 aren't both numbers
(define lub-helper
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2))
       ;; If we get here, something's wrong
       (error "oops!")]
      [(number? a1) a1]
      [(number? a2) a2]
      [else 'Bot])))

(define my-lub
  (lambda (p1 p2)
    (let ([car1 (car p1)]
          [cadr1 (cadr p1)]
          [car2 (car p2)]
          [cadr2 (cadr p2)])
      (cond
        ;; nat/Bot, nat/Bot
        ;; nat/Bot, nat/nat
        ;; nat/nat, nat/Bot
        ;; nat/nat, nat/nat
        [(and (number? car1) (number? car2))
         'Top]

        ;; Bot/nat, Bot/nat
        ;; nat/nat, Bot/nat
        ;; Bot/nat, nat/nat
        [(and (number? cadr1) (number? cadr2))
         'Top]

        ;; nat/Bot, Bot/nat
        ;; Bot/nat, nat/Bot
        [else (list
               (lub-helper car1 car2)
               (lub-helper cadr1 cadr2))]))))



