(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; (define (div-interval x y)
;   (mul-interval
;     x
;     (make-interval (/ 1.0 (upper-bound y))
;                    (/ 1.0 (lower-bound y)))))

; 2.7

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

; 2.8
; The lowest value the difference can be is the difference of the two first lower and second upper bounds
; The highest value the difference can be is the difference of the first upper and second lower bounds

; (define (sub-interval x y)
;   (make-interval
;     (- (lower-bound x) (upper-bound y))
;     (- (upper-bound x) (lower-bound y))))

; 2.9

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

; 2.9 test 
; (define i1 (make-interval 3.35 3.65))
; (define i2 (make-interval (- 3.0) 4.5))

; 2.10

(define (div-interval x y)
  (if (and (> (upper-bound y) 0) (< (lower-bound y) 0))
    (error "Divided by interval that spans 0" y)
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; 2.11

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    ; x2 > 0 IFF x1 > 0. Subsequently x2 < 0 only if x1 < 0
    (cond ((> x1 0)
            (cond ((> y1 0) (make-interval (* x1 y1) (* x2 y2)))
                  ((< y2 0) (make-interval (* x2 y1) (* x1 y2)))
                  (else (make-interval (* x2 y1) (* x2 y2)))))
          ((< x2 0)
            (cond ((> y1 0) (make-interval (* x1 y2) (* x2 y1)))
                  ((< y2 0) (make-interval (* x2 y2) (* x1 y1)))
                  (else (make-interval (* x1 y2) (* x1 y1)))))
          (else
            (cond ((> y1 0) (make-interval (* x1 y2) (* x2 y2)))
                  ((< y2 0) (make-interval (* x2 y1) (* x1 y1)))
                  (else (make-interval 
                          (min (* x1 y2) (* x2 y1))
                          (max (* x1 y1) (* x2 y2)))))))))

; 2.12

(define (make-center-percent c p)
  (let ((width (/ (* c p) 100.0)))
    (make-interval (- c width) (+ c width))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* (/ (width i) (center i)) 100.0))

; test
(define test-center (make-center-percent 100 5))

; 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; 2.14 test data
(define ic1 (make-center-percent 3.5 0.1)) ; - (2.54459146543169 . 2.6413876758854924)
(define ic2 (make-center-percent 10 1)) ; - (2.583909976486396 . 2.601194545521373)

; A/A should be (1, 1) with no uncertainty. par1 gives 1.86%, par2 gives 0.33%