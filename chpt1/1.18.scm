; p + ab = result
; 7 * 8 = 56:
; 0 + 7*8 = 56
; 0 + 14*4 = 56
; 0 + 28*2 = 56
; 0 + 56*1 = 56
; 56 + 56*0 = 56

(define (* a b)
  (mult-iter a b 0))

(define (mult-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mult-iter (double a) (halve b) p))
        (else (mult-iter a (- b 1) (+ a p)))))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))