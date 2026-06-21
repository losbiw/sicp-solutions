#lang sicp
(#%require "stream.rkt")


; part a
(define (integrate-series s)
  (stream-map / s integers))

; (display-n-stream (integrate-series ones) 5)

; part b

; cosine coefficients: 1, -1/2, 1/24, ...

(define cosine-series
  (cons-stream 1 (negate-stream (integrate-series sine-series))))


; sine coefficients: 0, 1, -1/6, 120, ...

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display-n-stream sine-series 8)
(display-n-stream cosine-series 8)

(#%provide cosine-series)
(#%provide sine-series)
