(define (cont-frac n d k)
  (define (step i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (step (+ i 1))))))
    
  (step 1))

(define (tan-cf x k)
  (cont-frac
    (lambda (i)
      (if (= i 1) x (* x x -1)))
    (lambda (i)
      (- (* 2 i) 1))
    k))