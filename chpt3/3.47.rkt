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

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; part a

(define (make-semaphore-from-mutex n)
  (let ((issued-mutex-count 0))
    (define (acquire-semaphore)
      (if (< issued-mutex-count n)
          (let ((process-mutex (make-mutex)))
            (process-mutex 'acquire)
            (set! issued-mutex-count
                  (+ issued-mutex-count 1))
            process-mutex)
          (acquire-semaphore)))

    (define (release-semaphore mutex)
      (mutex 'release)
      (set! issued-mutex-count
            (- issued-mutex-count 1)))
    
    (define (semaphore m)
      (cond ((eq? m 'acquire) acquire-semaphore)
            ((eq? m 'release) release-semaphore)))

    semaphore))

; part b

;(define (make-semaphore n)
;  ())

; 1. A semaphore can be acquired or released
; 2. A semaphore of size n can be acquired n times
; 3. A semaphore shouldn't need to know anything about its mutexes