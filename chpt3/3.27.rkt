#lang sicp

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (mem-fib n)
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (mem-fib (- n 1))
                            (mem-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((pre-computed-result (lookup table x)))
        (or pre-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define (execute-and-measure-time f . args)
  (let ((start-time (runtime)))
    (apply f args)
    (newline)
    (display (- (runtime) start-time))))

(execute-and-measure-time fib 3)
(execute-and-measure-time mem-fib 3)