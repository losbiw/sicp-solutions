(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))