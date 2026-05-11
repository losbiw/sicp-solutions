(define (smallest-divisor n)
  (divisor-iter n 2))

(define (divisor-iter n i)
  (cond ((> (square i) n) n)
        ((divides? n i) i)
        (else (divisor-iter n (+ i 1)))))

(define (divides? a b)
  (= (remainder a b) 0))


; 199 - 199
; 1999 - 1999
; 19999 - 7