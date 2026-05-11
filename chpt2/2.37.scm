(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial items)
  (if (null? items)
    initial
    (op (car items) (accumulate op initial (cdr items)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons () m))

; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map
;       (lambda (v)
;         (accumulate (lambda (x y) (cons (dot-product x v) y))
;                     ()
;                     cols))
;       m)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (r) (map (lambda (c) (dot-product r c)) cols))
      m)))

(define ma (list (list 4 3 1) (list (- 5) 7 2) (list 3 2 6)))
(define mb (list (list 1 2 2) (list (- 4) 3 (- 7)) (list 4 6 (- 8))))