#lang sicp

(define (make-account balance password)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient balance" balance)))

  (lambda (pwd op)
    (if (eq? pwd password)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "Unknown operation" op)))
        (error "Incorrect password"))))

(define acc (make-account 100 '123))

((acc '123 'deposit) 100)
((acc '12 'withdraw) 200)