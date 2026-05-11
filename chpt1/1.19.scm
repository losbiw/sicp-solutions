; Tpq: a <- bq + aq + ap, b <- bp + aq

; First transformation:
;   a1 = b0q + a0q + a0p
;   b1 = b0p + a0q

; Second transformation:
;   a2 = b1q + a1q + a1p = (b0p + a0q)q + (b0q + a0q + a0p)q + (b0q + a0q + a0p)p = b0pq + a0q^2 + b0q^2 + a0q^2 + a0pq + b0pq + a0pq + a0p^2 = 2a0q^2 + a0p^2 +2a0pq + b0q^2 + 2b0pq = b0(q^2 + 2pq) + a0(q^2 + 2pq) + a0(q^2 + p^2) 
;   b2 = b1p + a1q = (b0p + a0q)p + (b0q + a0q + a0p)q = b0p^2 + a0pq + b0q^2 + a0q^2 + a0pq = b0p^2 + b0q^2 + a0q^2 + 2a0pq = b0(p^2 + q^2) + a0(q^2 + 2pq)

; p' = p^2 + q^2
; q' = q^2 + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) 
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))