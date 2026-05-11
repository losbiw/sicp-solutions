(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

; common methods
(define (segment-len start-p end-p)
  (sqrt
    (+ (expt (- (x-point end-p) (x-point start-p)) 2) (expt (- (y-point end-p) (y-point start-p)) 2))))

(define (perimeter rect)
  (+ (* 2 (height rect)) (* 2 (width rect))))

(define (area rect)
  (* (height rect) (width rect)))

; perpendicular lines implementation

; (define (make-rectangle height width)
;   (cons height width))

; (define (height rect)
;   (let ((height-seg (car rect)))
;     (segment-len
;       (start-segment height-seg)
;       (end-segment height-seg))))

; (define (width rect)
;   (let ((width-seg (cdr rect)))
;     (segment-len
;       (start-segment width-seg)
;       (end-segment width-seg))))

; (define test-rectangle
;   (make-rectangle
;     (make-segment 
;       (make-point 0 0)
;       (make-point 0 10))
;     (make-segment
;       (make-point 0 10)
;       (make-point 10 10))))

; 3 corners implementation
; c2 is the origin (arbitrarily)

(define (make-rectangle c1 c2 c3)
  (cons c2 (cons c1 c3)))

(define (origin-rect rect)
  (car rect))

(define (c1-rect rect)
  (car (cdr rect)))

(define (c3-rect rect)
  (car (cdr rect)))

(define (height rect)
  (segment-len (origin-rect rect) (c1-rect rect)))

(define (width rect)
  (segment-len (origin-rect rect) (c3-rect rect)))

(define test-rectangle
  (make-rectangle
    (make-point 0 0)
    (make-point 0 10)
    (make-point 10 10)))
