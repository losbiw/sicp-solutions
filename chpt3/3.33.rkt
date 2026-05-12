#lang sicp
(#%require "constraints.rkt")
(#%require "connectors.rkt")

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(define (averager a b c)
  (let ((s (make-connector))
        (h (make-connector)))
    (adder a b s)
    (constant 0.5 h)
    (multiplier s h c)
    'ok))

(averager A B C)
(probe "Average" C)

(set-value! A 10 'user)
(set-value! B 20 'user)
(forget-value! A 'user)
(set-value! A 30 'user)