(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

; Test
(define (square x) (* x x))

(newline)
(display ((repeated square 2) 5))
(newline)