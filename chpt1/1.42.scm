(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (compose f f))