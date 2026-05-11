#lang sicp

;; (define (equal? a b)
;;   (cond ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
;;                                         (equal? (cdr a) (cdr b))))
;;         ((and (not (pair? a)) (not (pair? b))) (eq? a b))
;;         (else #f)))

(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (and (not (pair? a))
           (not (pair? b))
           (eq? a b))))