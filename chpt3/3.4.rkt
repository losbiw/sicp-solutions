#lang sicp

(define (call-the-cops)
  (newline) (display "wee woo"))

(define (make-account balance password)
  (let ((unathorized-access-count 0))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient balance"))

    (lambda (pwd op)
      (lambda (amount)
        (if (eq? pwd password)
            (cond ((eq? op 'withdraw) (withdraw amount))
                  ((eq? op 'deposit) (deposit amount))
                  (else (error "Unknown operation" op)))
            (begin (set! unathorized-access-count (+ unathorized-access-count 1))
                   (if (> unathorized-access-count 7)
                       (call-the-cops)
                       "Incorrect password")))))))

(define acc (make-account 100 '123))

((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)
((acc '12 'withdraw) 200)