#lang sicp

(define (rand-update x) (random (expt 2 31)))

(define rand-init (rand-update 0))

(define rand
  (let ((x rand-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (set! x (rand-update x)) x)
            ((eq? action 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else (error "Unknown action -- RAND" action))))))