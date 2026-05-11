#lang sicp

;; generic 

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; part a

;; (define (sum? x)
;;   (and (pair? x) (eq? (cadr x) '+)))
;; 
;; (define (addend s) (car s))
;; 
;; (define (augend s) (caddr s))
;; 
;; (define (product? x)
;;   (and (pair? x) (eq? (cadr x) '*)))
;; 
;; (define (multiplier p) (car p))
;; 
;; (define (multiplicand p) (caddr p))
;; 
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else (list a1 '+ a2))))
;; 
;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list m1 '* m2))))

;; part b

(define (includes sym exp)
  (and (pair? exp) (memq sym exp)))

(define (unwrap exp)
  (if (and (pair? exp) (null? (cdr exp)))
      (car exp)
      exp))

(define (after sym exp)
  (unwrap (cdr (memq sym exp))))

(define (before sym exp)
  (define (iter rest)
    (if (eq? (car rest) sym)
        nil
        (cons (car rest) (iter (cdr rest)))))

  (unwrap (iter exp)))

(define (sum? x) (includes '+ x))

(define (addend s) (before '+ s))

(define (augend s) (after '+ s))

(define (product? x)
  (and (not (sum? x)) (includes '* x)))

(define (multiplier p) (before '* p))

(define (multiplicand p) (after '* p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else (error "Unknown expression type --DERIV" exp))))
