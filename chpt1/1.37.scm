(define (cont-frac n d k)
  (define (step i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (step (+ i 1))))))
    
  (step 1))

(define (cont-frac-linear n d k)
  (define (iter i sum)
    (if (= i 0)
      sum
      (iter (- i 1) (/ (n i) (+ (d i) sum)))))
    
  (iter k 0))

(define (phi k)
  (/ 1 (cont-frac-linear (lambda (i) 1.0) (lambda (i) 1.0) k)))

; k = 13 to achieve 4 decimal places accuracy