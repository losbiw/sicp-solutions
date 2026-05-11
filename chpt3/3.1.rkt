#lang sicp

(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

(define A (make-accumulator 5))
(define B (make-accumulator 0))

(A 10)
(A 10)
(B 100)