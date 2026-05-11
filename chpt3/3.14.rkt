#lang sicp

(define (reverse x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))

  (loop x '()))

(define v '(a b c d))
(define w (reverse v))
w
v