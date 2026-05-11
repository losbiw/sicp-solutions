(define (product a b term next)
  (if (> a b)
    1
    (* (term a) (product (next a) b term next))))

(define (product-linear a b term next)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
    
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))

  (product 1 n identity inc))

(define (approx-pi accuracy)
  (define (inc-by-two x) (+ x 2))

  (define (frac-term k)
    (* (/ k (+ k 1)) (/ (+ k 2) (+ k 1))))

  (* 4.0 (product-linear 2 accuracy frac-term inc-by-two)))