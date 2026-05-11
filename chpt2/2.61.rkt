#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (cons x nil))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Worst case scenario - x is bigger than all elements in set O(n)
; Best case scenario - x equals to/smaller than the first element: O(1)
; Average case: O(n/2) = O(n), but saves a factor of two