#lang sicp
(#%require "queue.rkt")

(define q1 (make-queue))

(insert-queue! q1 'a)

(insert-queue! q1 'b)

(print-queue q1)

(delete-queue! q1)

(delete-queue! q1)

(print-queue q1)

; When an item is delete from a queue that only contains one item, only the front pointer is reassigned. Therefore, the rear pointer still points to the old value, despite it not being in the queue anymore
; print-queue method is defined in queue.rkt