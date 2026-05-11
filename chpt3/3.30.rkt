#lang sicp
(#%require "3.29.rkt")
(#%require "wire.rkt")

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a b s c)
  (define (iter a b s ck-1)
    (if (null? a)
        'ok
        (let ((ak (car a)) (bk (car b)) (sk (car s)) (ck (make-wire)))
          (full-adder ak bk ck-1 sk ck)
          (iter (cdr a) (cdr b) (cdr s) ck))))
  
  (if (= (length a) (length b) (length s))
      (iter a b s c)
      (error "Lists of wires must be of same length -- RIPPLE-CARRY-ADDER" (list a b s))))

; Half adder: (max: or-delay, and-delay + inverter-delay) + and-delay

; Full adder: 2 * half-adder-delay + or-delay

; n-bit ripple-carry-adder: n * full-adder