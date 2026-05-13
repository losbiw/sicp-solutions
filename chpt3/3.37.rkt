#lang sicp

(#%require "constraints.rkt")
(#%require "connectors.rkt")

(define (c+ a b)
  (let ((z (make-connector)))
    (adder a b z)
    z))

(define (c* x y)
  (let ((p (make-connector)))
    (multiplier x y p)
    p))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv a)
  (let ((c (make-connector)))
    (constant a c)
    c))

(define (celsius-to-fahrenheit x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-to-fahrenheit C))
(probe "Fahrenheit" F)

(set-value! C 100 'user)