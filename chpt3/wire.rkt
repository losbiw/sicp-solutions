#lang sicp

; wire implementation

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown Expression -- WIRE" m))))

    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action)
  ((wire 'add-action!) action))

; miscellaneous

(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))

; exports

(#%provide make-wire)
(#%provide get-signal)
(#%provide set-signal!)
(#%provide add-action!)