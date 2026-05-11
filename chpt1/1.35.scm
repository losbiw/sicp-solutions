(define (fixed-point f guess tolerance)
  (define (close-enough a b)
    (< (abs (- a b)) tolerance))
    
  (define (try current-guess)
    (let ((next-guess (f current-guess)))
      (if (close-enough current-guess next-guess)
        next-guess
        (try next-guess))))
    
  (try guess))