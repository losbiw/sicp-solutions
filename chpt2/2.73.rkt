#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (install-sum-package)
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (put 'deriv '(+) sum-deriv))

(define (install-product-package)
  (define (product-deriv operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (multiplicand operands)
                   (deriv (multiplier operands) var))))

  (put 'deriv '(*) product-deriv))

(define (install-exponent-package)
  (define (exponent-deriv operands exp)
    (make-product
     (make-product
      (exponent operands)
      (make-exponent
       (base operands)
       (- (exponent operands) 1)))
     (deriv (base operands) var)))

  (put 'deriv '(**) exponent-deriv))

(define get nil)
(define put nil)