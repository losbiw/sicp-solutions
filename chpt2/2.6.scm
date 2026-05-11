(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ n m)
  (lambda (f)
    (lambda (x) ((n f) ((m f) x)))))

; Addition, have:
; (lambda (f) (lambda (x) (f x)))
; +
; (lambda (f) (lambda (x) (f (f x))))
; should =
; (lambda (f) (lambda (x) (f (f (f x)))))