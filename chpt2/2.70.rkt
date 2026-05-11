#lang sicp
(#%require "2.69.rkt")
(#%require "2.68.rkt")

(define song-tree
  (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))

(define lyrics '(GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                      SHA BOOM))

(define encoded (encode lyrics song-tree))

; length: 84 bits
; with fixed-length: 3 bits (per character (8 different characters), 36 total characters
; total fixed length: 36 * 3 = 108
; saving 24 bits