#lang sicp

(define (make-wire) nil)
(define (get-signal wire) nil)
(define (set-signal! wire new-value) nil)
(define (add-action! wire action) nil)
(define (after-delay delay proc) nil)
(define or-gate-delay 5)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure))

(define (logical-or o1 o2)
  (if (= o1 o2 0)
      0
      1))