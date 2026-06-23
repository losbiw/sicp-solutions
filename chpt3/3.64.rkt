#lang sicp
(#%require "stream.rkt")

(define (stream-limit s tolerance)
  (if (stream-null? (stream-cdr s))
      nil
      (let ((s0 (stream-car s))
            (s1 (stream-ref s 1)))
        (if (< (abs (- s0 s1)) tolerance)
            s1
            (stream-limit (stream-cdr s)
                          tolerance)))))

; test

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess))
     2.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))

  guesses)

(define (sqrt x)
  (stream-limit (sqrt-stream x) 0.001))

(sqrt 13)