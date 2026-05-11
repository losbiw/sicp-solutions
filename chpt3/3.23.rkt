#lang sicp

; the implementation should involve a doubly-linked list
(define (make-node item prev next) (cons item (cons prev next)))

(define (data-node node) (car node))

(define (prev-node node) (cadr node))
(define (next-node node) (cddr node))
(define (set-prev! node prev) (set-car! (cdr node) prev))
(define (set-next! node next) (set-cdr! (cdr node) next))

; deque implementation

(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with empty deque" deque)
      (data-node (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with empty deque" deque)
      (data-node (rear-ptr deque))))

(define (insert-front-deque! deque item)
  (let ((new-node (make-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-prev! (front-ptr deque) new-node)
           (set-next! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)
           deque))))

(define (insert-rear-deque! deque item)
  (let ((new-node (make-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-prev! new-node (rear-ptr deque))
           (set-next! (rear-ptr deque) new-node)
           (set-rear-ptr! deque new-node)
           deque))))

(define (delete-front-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE-FRONT called with empty deque" deque))
        (else
         (set-front-ptr! deque (next-node (front-ptr deque)))
         (set-prev! (front-ptr deque) '())
         deque)))

(define (delete-rear-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE-REAR called with empty deque" deque))
        (else
         (set-rear-ptr! deque (prev-node (rear-ptr deque)))
         (set-next! (rear-ptr deque) '())
         deque)))

(define d1 (make-deque))
(insert-rear-deque! d1 'a)
(insert-rear-deque! d1 'b)
(insert-rear-deque! d1 'c)
(insert-front-deque! d1 'd)

(delete-rear-deque! d1)
(delete-front-deque! d1)