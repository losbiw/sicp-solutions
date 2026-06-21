#lang sicp
(#%require "stream.rkt")
(#%require "3.59.rkt")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(display-n-stream (add-streams (mul-series cosine-series cosine-series)
                               (mul-series sine-series sine-series))
                  10)

(#%provide mul-series)