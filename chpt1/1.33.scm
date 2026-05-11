(define (filtered-accumulate combiner null-value filter term a next b)
  (cond ((> a b) null-value)
        ((filter a) 
          (combiner
            (term a)
            (filtered-accumulate combiner null-value filter term (next a) next b)))
        (else (filtered-accumulate combiner null-value filter term (next a) next b))))

(define (sum-primes-squared a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (relative-primes-product b)
  (define (identity x) x)
  (define (relative-filter a)
    (relative-primes a b))

  (filtered-accumulate * 1 relative-filter identity 1 inc (- b 1)))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (relative-primes a b)
  (= (gcd a b) 1))

(define (inc x) (+ x 1))

(define (divisor-iter n i)
  (define (next)
    (if (= i 2)
      3
      (+ i 2)))

  (cond ((> (square i) n) n)
        ((= (remainder n i) 0) i)
        (else (divisor-iter n (next)))))

(define (smallest-divisor n)
  (divisor-iter n 2))

(define (prime? n)
  (= (smallest-divisor n) n))
