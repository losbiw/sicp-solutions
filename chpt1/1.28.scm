(define (miller-test n)
  (define (try-random a)
    (= (expmod a (- n 1) n) 1))

  (try-random (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (define (triviality-step root)
    (triviality-check root (remainder (square root) m)))

  (define (triviality-check root modulo)
    (if (and (not (or (= root 1) (= root (- m 1)))) (= modulo 1))
      0
      modulo))

  (cond ((= exp 0) 1)
        ((even? exp) (triviality-step (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))