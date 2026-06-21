#lang sicp
(#%require "stream.rkt")

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

; Interval enumeration: 0
; Stream-ref 5: 1 - 5
; Stream-ref 6: 6, 7
; 