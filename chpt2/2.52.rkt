#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((right (right-split painter (- n 1))))
      (beside painter (below right right)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  ((square-of-four flip-horiz
                   identity
                   rotate180
                   flip-vert )
   (corner-split painter n)))

(define wave
  (segments->painter (list (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
                           (make-segment (make-vect 0.6 0) (make-vect 0.5 0.3))
                           (make-segment (make-vect 0.3 0) (make-vect 0.4 0.6))
                           (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.65))
                           (make-segment (make-vect 0.3 0.65) (make-vect 0.2 0.5))
                           (make-segment (make-vect 0.2 0.5) (make-vect 0 0.6))
                           (make-segment (make-vect 0 0.7) (make-vect 0.15 0.65))
                           (make-segment (make-vect 0.15 0.65) (make-vect 0.275 0.7))
                           (make-segment (make-vect 0.275 0.7) (make-vect 0.425 0.725))
                           (make-segment (make-vect 0.425 0.725) (make-vect 0.4 0.9))
                           (make-segment (make-vect 0.4 0.9) (make-vect 0.45 1))
                           (make-segment (make-vect 0.55 1) (make-vect 0.6 0.9))
                           (make-segment (make-vect 0.6 0.9) (make-vect 0.575 0.725))
                           (make-segment (make-vect 0.575 0.725) (make-vect 0.7 0.725))
                           (make-segment (make-vect 0.7 0.725) (make-vect 1 0.4))
                           (make-segment (make-vect 1 0.25) (make-vect 0.6 0.6))
                           (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0))
                           (make-segment (make-vect 0.45 0.8) (make-vect 0.55 0.8)))))