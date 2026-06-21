#lang sicp
(#%require "stream.rkt")

; accumulated sums: (1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210)

(define sum 0)
; sum = 0
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; only the first element of the sequence is evaluated
; sum = 1

(define y (stream-filter even? seq))
; filter evaluates until it finds one element that passes the predicate
; sum = 1 + 2 + 3 = 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; again, evaluates until it finds an item divisible by 5
; sum = 6 + 4 = 10

(stream-ref y 7)
; y, so looking for 8th even accumulated sum (0-based indexing)
; (6, 10, 28, 36, 66, 78, 120, 136, ...)
; sum = 136

(display-stream z)
; (10, 15, 45, 105, 120, 190, 210)
; sum = 210

; Yes, the responses would differ without memoization, since each new access to the stream car/cdr would reevaluate previously evaluated members of the sequence, thus increasing accumulated sum