(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (coeff higher-terms) (+ coeff (* x higher-terms)))
              0
              coefficient-sequence))