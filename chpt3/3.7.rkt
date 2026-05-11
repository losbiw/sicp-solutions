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

(define (make-joint acc main-password joint-password)
  (lambda (pwd op)
    (if (eq? pwd joint-password)
        (acc main-password op)
        (error "Incorrect joint password"))))

(define main-acc (make-account 0 'pwd1))
(define joint-acc (make-joint main-acc 'pwd1 'pwd2))

((main-acc 'pwd1 'deposit) 100)
((joint-acc 'pwd2 'withdraw) 20)