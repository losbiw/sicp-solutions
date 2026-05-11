#lang sicp

(define (count-pairs x)
  (let ((seen '()))
    (define (iter x)
      (if (or (not (pair? x)) (memq x seen))
          0
          (begin (set! seen (cons x seen))
                 (+ (iter (car x))
                    (iter (cdr x))
                    1))))

    (iter x)))

(define x (cons 'a 'b))
(define 3-4 (cons x (cons x nil)))
(count-pairs 3-4)

(define y (cons x x))
(define 3-7 (cons y y))
(count-pairs 3-7)