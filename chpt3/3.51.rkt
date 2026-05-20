#lang sicp
(#%require "stream.rkt")

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

; Prediction: defining x prints out number from 0 through 10 and results in the following stream: (stream-cons 0 (delay stream-enumerate-interval 1 10))
; stream-ref x 5 shows 5
; stream-ref x 7 shows 7