#lang sicp
(#%require sicp-pict)

(define (split composer split-composer)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split composer split-composer) painter (- n 1))))
          (composer painter (split-composer smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

(paint (right-split einstein 10))