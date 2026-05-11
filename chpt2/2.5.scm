(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (even? x) (= (remainder x 2) 0))

(define (log-b x base)
  (/ (log x) (log base)))

(define (car z)  
  (if (= (remainder z 3) 0)
    (car (/ z 3))
    (log-b z 2)))

(define (cdr z)
  (if (even? z)
    (cdr (/ z 2))
    (log-b z 3)))