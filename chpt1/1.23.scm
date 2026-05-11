(define (smallest-divisor n)
  (divisor-iter n 2))

(define (divisor-iter n i)
  (cond ((> (square i) n) n)
        ((divides? n i) i)
        (else (divisor-iter n (next i)))))

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes min max)
  (prime-search-iter min max (+ min 1)))

(define (prime-search-iter min max n)
  (define (test-and-proceed)
    (timed-prime-test n)
    (prime-search-iter min max (+ n 1)))

  (if (and (> n min) (< n max))
    (test-and-proceed)))

; 100000000 - 0.009, 0.009, 0.009. avg: 0.009
; 1000000000 - 0.02, 0.02, 0.19. avg: 0.02
; 10000000000 - 0.06, 0.06, 0.06. avg: 0.06

; 1 to 2: 2.2 longer
; 2 to 3: 3 longer

; Roughly the same
; Supposed to be about 1.7 times faster, not on my machine. The missing 30% is explained by extra overhead (an extra if statement)