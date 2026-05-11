#lang sicp

(define (=zero? x)
  (apply-generic '=zero? x))

;...
(put '=zero? ('scheme-number) zero?)

;...
(define (=zero? x) (zero? (numer x)))
(put '=zero? ('rational) =zero?)

;...
(define (=zero? z) (zero? (magnitude z)))
(put '=zero? ('complex) =zero?)