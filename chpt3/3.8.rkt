#lang sicp

(define f
  (let ((val #f))
    (lambda (arg)
      (if (not val)
          (set! val arg))
      val)))

(f 0)
(f 1)