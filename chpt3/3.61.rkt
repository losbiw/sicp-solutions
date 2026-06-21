#lang sicp
(#%require "stream.rkt")
(#%require "3.60.rkt")

(define (invert-unit-series s)
  (cons-stream 1 (negate-stream (mul-series (stream-cdr s)
                                            (invert-unit-series s)))))