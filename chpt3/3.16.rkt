#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define 3-3 (cons 'a (cons 'b (cons 'c nil))))
(count-pairs 3-3)

(define x (cons 'a 'b))
(define 3-4 (cons x (cons x nil)))
(count-pairs 3-4)

(define y (cons x x))
(define 3-7 (cons y y))
(count-pairs 3-7)

(define z (cons nil nil))
(define 3- (cons z x))
(set-car! z 3-)
(count-pairs 3-)