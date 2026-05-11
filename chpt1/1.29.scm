(define (simpsons f a b n)
  (simpsons-init f a b n (/ (- b a) n)))

(define (simpsons-init f a b n h)
  (define (increase x) (+ x 1))

  (define (y k) 
    (* (cond ((or (= k 0) (= k (- n 1))) 1)
             ((even? k) 2)
             (else 4))
        (f (+ a (* k h)))))

  (* (sum 0 n y increase)
     (/ h 3.0)))

(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a) (sum (next a) b term next))))

(define (cube x) (* x x x))