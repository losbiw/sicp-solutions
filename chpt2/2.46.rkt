#lang sicp

(define (make-vect x y)
  (cons x y))

(define (x-cor v)
  (car v))

(define (y-cor v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (x-cor v1) (x-cor v2))
             (+ (y-cor v1) (y-cor v2))))

(define (sub-vect v1 v2)
  (make-vect (- (x-cor v1) (x-cor v2))
             (- (y-cor v1) (y-cor v2))))

(define (scale-vect vect scalar)
  (make-vector (* scalar (x-cor vect))
               (* scalar (y-cor vect))))

