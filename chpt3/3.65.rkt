#lang sicp
(#%require "stream.rkt")

; a helper func
(define (square x) (* x x))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(newline)
(display "Naive Implementation")
(display-n-stream ln2-stream 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(newline)
(display "Singular Euler transform")
(display-n-stream (euler-transform ln2-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(newline)
(display "Recursive Eurler transform")
(display-n-stream (accelerated-sequence euler-transform
                                        ln2-stream)
                  10)

; obviously, the naive implementation is the slowest to converge, followed by euler transform and the accelerated sequence