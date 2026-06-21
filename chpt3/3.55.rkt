#lang sicp
(#%require "stream.rkt")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define p (partial-sums integers))

(display-n-stream p 5)


; Similar to the fibonacci example in the book

;   2 3 4  ... = (stream-cdr integers)
;   1 3 6  ... = (partial-sums s)
; -------------
; 1 3 6 10 ... = (partial-sum s)
