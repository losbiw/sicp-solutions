#lang sicp

(define (has-cycle? x)
  (define (safe-cddr x)
    (if (not (pair? (cdr x)))
        nil
        (cddr x)))
  
  (define (iter slow fast)
    (cond ((eq? slow fast) #t)
          ((or (not (pair? slow))
               (not (pair? fast)))
           #f)
          (else (iter (cdr slow) (safe-cddr fast)))))

  (iter (cdr x) (cddr x)))

; test

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define x (list 1 2 3))
(has-cycle? (make-cycle x))

(define y (list 1 2 3))
(has-cycle? y)