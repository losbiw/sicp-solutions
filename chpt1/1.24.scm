(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-random a)
    (= (expmod a n n) a))
    
  (try-random (+ 1 (random (- n 1)))))

(define (expmod base expt m)
  (cond ((= expt 0) 1)
        ((even? expt) 
          (remainder (square (expmod base (/ expt 2) m)) m))
        (else (remainder (* base (expmod base (- expt 1) m)) m))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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

; Timing data not accurate enough
; seems to be much much faster. 0 seconds for each level