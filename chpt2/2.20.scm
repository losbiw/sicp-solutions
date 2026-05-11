(define (same-parity v . z)
  (define (iter pred result)
    (cond ((null? result) ())
          ((pred (car result)) (cons (car result) (iter pred (cdr result))))
          (else (iter pred (cdr result)))))

  (cond ((null? z) (list v))
        ((even? v) (iter even? (cons v z)))
        (else (iter odd? (cons v z)))))