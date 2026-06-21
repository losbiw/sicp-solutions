#lang sicp
(#%require "stream.rkt")

(define factorials (cons-stream 1 (mul-streams factorials
                                               (integers-starting-from 2))))

(stream-cdr (stream-cdr (stream-cdr factorials))) ; n=3, res = 4! = 24
