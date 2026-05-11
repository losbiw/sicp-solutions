(define (fixed-point f first-guess)
  (define tolerance 0.000001)

  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
    
  (define (try guess)
    (let ((new-guess (f guess)))
      (if (close-enough? guess new-guess)
        new-guess
        (try new-guess))))
        
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point 
    ((repeated average-damp (floor (log2 n)))
      (lambda (y) (/ x (expt y (- n 1)))))
    1))

; Roots | Damps
; 2 - 1
; 3 - 1
; 4 - 2
; 5 - 2
; 6 - 2
; 7 - 2
; 8 - 3
; n - log2(n) to the nearest lower integer