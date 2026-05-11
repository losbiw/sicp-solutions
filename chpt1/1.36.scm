(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))

  (define (print-and-retry guess)
    (display guess)
    (newline)
    (try guess))
    
  (define (try guess)
    (let ((new-guess (f guess)))
      (if (close-enough? guess new-guess)
        new-guess
        (print-and-retry new-guess))))
    
  (try first-guess))

; Without avg damping: 34
; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2) 

; With avg damping: 9
; (fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) 2)