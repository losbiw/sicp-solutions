#lang sicp
(#%require "wire.rkt")
(#%require "agenda.rkt")

(define inverter-delay 2)
(define and-gate-delay 3)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! input invert-input))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid Syntax" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (logical-and s1 s2)
  (if (= s1 s2 1)
      1
      0))

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((c (make-wire))
          (d (make-wire))
          (e (make-wire)))
      (inverter o1 c)
      (inverter o2 d)
      (and c d e)
      (inverter e output)))

  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure))

; the delay is 2x inverter-delay + 1 and-delay


(#%provide inverter)
(#%provide and-gate)
(#%provide or-gate)