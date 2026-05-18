#lang sicp
(#%require "3.47.rkt")

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))

      serialized-p)))

; account

(define (id-generator)
  (let ((count 0))
    (define (increment)
      (set! count (+ count 1))
      count)

    increment))

(define next-id (id-generator))

(define (make-account-and-serializer balance)
  (let ((id (next-id)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'id) id)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

; exchange

(define (serialized-exchange a1 a2)
  (let ((id1 (a1 'id))
        (id2 (a2 'id))
        (s1 (a1 'serializer))
        (s2 (a2 'serializer)))
    (cond ((< id1 id2) ((s2 (s1 exchange))
                        a1
                        a2))
          ((> id1 id2) ((s1 (s2 exchange))
                        a1
                        a2))
          (else (error "Account IDs cannot be equal -- SERIALIZED-EXCHANGE"
                       (list a1 a2))))))

(define (exchange a1 a2)
  (let ((difference (- (a1 'balance)
                       (a2 'balance))))
    ((a1 'withdraw) difference)
    ((a2 'deposit) difference)))
