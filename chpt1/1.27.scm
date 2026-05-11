(define (carmichael n)
  (carmichael-iter n (- n 1)))

(define (carmichael-iter n a) 
  (cond ((= a 1) true)
        ((fermat-test n a) (carmichael-iter n (- a 1)))
        (else false)))

(define (fermat-test n a) 
  (= (expmod a n n) a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))