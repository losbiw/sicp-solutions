(define (gcd a b)
  (if (= b 0)
    ; abs makes it work with negative numbers pretty sure
    (abs a)
    (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (denom-sign (if (< d 0) - +)))
    (cons
      (denom-sign (/ n g))
      (denom-sign (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define test (make-rat (- 7) 3))