#lang sicp
(#%require "queue.rkt")

; time segments

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

; agenda implementation

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belong-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= time (segment-time (car segments)))
        (insert-queue! (segment-queue (car segments)
                                      action))
        (let ((rest (cdr segments)))
          (if (belong-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belong-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment)
                             segments))
        (add-to-segments! segments))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Empty agenda -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; agenda instance

(define the-agenda (make-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item!)
        (propagate))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; exports

(#%provide after-delay)