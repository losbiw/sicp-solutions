#lang sicp

(define (equ? a b)
  (apply-generic 'equ? a b))

(define (install-scheme-numbers)
  (put 'equ? '(scheme-number scheme-number) =))

(define (install-rational-numbers)
  (define (equ? a b)
    (and (= (numerator a) (numerator b))
         (= (denom a) (denom b))))

  (put 'equ? '(rational rational) equ?))

(define (install-complex-numbers)
  (define (equ? a b)
    (and (= (magnitude a) (magnitude b))
         (= (angle a) (angle b))))

  (put 'equ? '(complex complex) equ?))