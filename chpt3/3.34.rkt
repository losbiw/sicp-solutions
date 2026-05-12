#lang sicp
(#%require "constraints.rkt")
(#%require "connectors.rkt")

(define (squarer a b)
  (multiplier a a b))

(define A (make-connector))
(define B (make-connector))

(squarer A B)
(probe "x" A)
(probe "x^2" B)

(set-value! A 2 'user)
(forget-value! A 'user)
(set-value! A 5 'user)
(forget-value! A 'user)

(set-value! B 25 'user)