#lang sicp
(#%require "constraints.rkt")
(#%require "connectors.rkt")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (expt (get-value a) 2) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request -- SQUARER" request))))

  (connect a me)
  (connect b me)
  me)

; Test

(define A (make-connector))
(define B (make-connector))

(squarer A B)
(probe "x" A)
(probe "x^2" B)

(set-value! A 4 'user)
(forget-value! A 'user)

(set-value! B 25 'user)
(forget-value! B 'user)

(set-value! B -2 'user)