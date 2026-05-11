(define (iterative-improve good-enough? improve)
  (lambda (guess)
      (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define tolerance 0.000001)

(define (average-two a b)
  (/ (+ a b) 2.0))

; Sqrt

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) tolerance))

  (define (improve guess)
    (average-two guess (/ x guess)))

  ((iterative-improve good-enough? improve) x))

; Fixed-point

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))

  ((iterative-improve good-enough? f) first-guess))