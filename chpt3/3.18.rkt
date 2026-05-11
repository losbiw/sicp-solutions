#lang sicp

(define (has-cycle? x)
  (define (iter x seen)
    (cond ((not (pair? x)) #f)
          ((memq x seen) #t)
          (else (iter (cdr x) (cons x seen)))))

  (iter x '()))

; test

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x (list 1 2 3))
(has-cycle? (make-cycle x))