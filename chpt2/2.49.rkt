#lang sicp
(#%require sicp-pict)

;; (define (for-each proc items)
;;   (cond ((null? items) #t)
;;         (else (proc (car items))
;;               (for-each proc (cdr items)))))
;; 
;; (define (frame-coord-map frame)
;;   (lambda (vector)
;;     (add-vect (origin-frame frame)
;;               (add-vect (scale-vect (x-cor vector)
;;                                     (edge1-frame frame))
;;                         (scale-vect (y-cor vector)
;;                                     (edge2-frame frame))))))

;; (define (segments->painter segment-list)
;;   (lambda (frame)
;;     (for-each
;;      (lambda (segment)
;;        (let ((coord-map (frame-coord-map frame)))
;;          (draw-line (coord-map (start-segment segment))
;;                     (coord-map (end-segment segment))))
;;       segment-list))))

(define outline
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 0) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1)))))

(define x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond
  (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                           (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

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
                           (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0)))))