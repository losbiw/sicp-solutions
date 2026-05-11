(define (cont-frac n d k)
  (define (step i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (step (+ i 1))))))
    
  (step 1))

(define (e)
  (+ 2 (cont-frac
    (lambda (i) 1.0)
    (lambda (i)
      (if (= (remainder i 3) 2) 
        (* 2 (/ (+ i 1) 3))
        1.0))
    20)))