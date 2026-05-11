#lang sicp

(define (make-monitored f)
  (let ((counter 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) counter)
            ((eq? arg 'reset-counter) (begin (set! counter 0)
                                             counter))
            (else (begin (set! counter (+ counter 1))
                         (f arg)))))))

(define mf (make-monitored sqrt))
(mf 4)
(mf 10)
(mf 16)
(mf 'how-many-calls?)