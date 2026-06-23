#lang sicp
(#%require "stream.rkt")
(#%require "series.rkt")
; sine + cosine series
(#%require "3.59.rkt")

(define (invert-series s)
  (let ((const-term (stream-car s)))
    (if (= const-term 0)
        (error "constant term is 0 -- INVERT-SERIES" s)
        (cons-stream
         (/ 1 const-term)
         (scale-stream (negate-stream
                        (mul-series (stream-cdr s)
                                    (invert-series s)))
                       (/ const-term))))))

(define (div-series num denom)
  (mul-series num
              (invert-series denom)))

(define tangent-series
  (div-series sine-series
              cosine-series))

(display-n-stream tangent-series 8)