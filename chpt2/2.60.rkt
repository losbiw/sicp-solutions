#lang sicp

; O(n), used to be O(n)
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; O(1), used to be O(n)
(define (adjoin-set x set)
  (cons x set))

; O(n), used to be O(n^2)
(define (union-set set1 set2)
  (append set1 set2))

; Programs where the count of each element's occurence is important
; Also more efficient for apps that moslty adjoin and and get union-sets, instead of intersections