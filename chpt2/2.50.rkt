#lang sicp
(#%require sicp-pict)
 
(define (frame-coord-map frame)
  (lambda (vector)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor vector)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor vector)
                                          (frame-edge2 frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((frame-map (frame-coord-map frame)))
      (let ((new-origin (frame-map origin)))
        (painter (make-frame new-origin
                    (vector-sub (frame-map corner1) new-origin)
                    (vector-sub (frame-map corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))