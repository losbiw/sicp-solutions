#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))
            (else (error "Unknown action -- MAKE-MUTEX" m))))

    mutex))

(#%provide make-mutex)

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; part a

(define (make-semaphore-from-mutex n)
  (let ((count-mutex (make-mutex))
        (count 0))
    (define (acquire-semaphore)
      (count-mutex 'acquire)
      (if (< count n)
          (begin (set! count (+ count 1))
                 (count-mutex 'release))
          (begin (count-mutex 'release)
                 (acquire-semaphore))))

    (define (release-semaphore)
      (count-mutex 'acquire)
      (set! count (- count 1))
      (count-mutex 'release))
    
    (define (semaphore m)
      (cond ((eq? m 'acquire) (acquire-semaphore))
            ((eq? m 'release) (release-semaphore))
            (else (error "Unknown action -- SEMAPHORE" m))))

    semaphore))

; part b

(define (make-semaphore n)
  (let ((count 0)
        (cell (list false)))
    (define (acquire-semaphore)
      (if (test-and-set! cell)
          (acquire-semaphore)
          (if (< count n)
              (begin (set! count (+ count 1))
                     (clear! cell))
              (begin (clear! cell)
                     (acquire-semaphore)))))

    (define (release-semaphore)
      (if (test-and-set! cell)
          (release-semaphore)
          (begin (set! count (- count 1))
                 (clear! cell))))
  
    (define (semaphore m)
      (cond ((eq? m 'acquire) (acquire-semaphore))
            ((eq? m 'release) (release-semaphore))
            (else (error "Unknown action -- SEMAPHORE" m))))

    semaphore))

; 1. A semaphore can be acquired or released
; 2. A semaphore of size n can be acquired n times
; 3. A semaphore cannot be acquired/released by multiple consumers at once