#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (set-front-ptr! item) (set! front-ptr item))

    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (empty?) (null? front-ptr))

    (define (insert item)
      (let ((new-item (cons item '())))
        (cond ((empty?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item))
              (else
               (set-cdr! rear-ptr new-item)
               (set-rear-ptr! new-item)
               dispatch))))

    (define (delete)
      (cond ((empty?)
             (error "DELETE with empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             dispatch)))

    (define (print)
      (newline)
      (display front-ptr))
    
    (define (dispatch m)
      (cond ((eq? m 'insert) insert)
            ((eq? m 'delete) (delete))
            ((eq? m 'print) (print))
            (else
             (error "unknown action -- QUEUE-DISPATCH" (list m front-ptr)))))

    dispatch))

(define q1 (make-queue))

((q1 'insert)'a)

((q1 'insert)'b)

(q1 'print)

(q1 'delete)

(q1 'delete)

(q1 'print)