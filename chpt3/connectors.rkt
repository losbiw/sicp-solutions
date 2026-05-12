#lang sicp

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'get-value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector informant)
  ((connector 'forget-value!) informant))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; dispatch

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'get-value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget-value!) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown request -- MAKE-CONNECTOR" request))))

    me))

(define (for-each-except exception proc list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (proc (car items))
                (loop (cdr items)))))

  (loop list))

; exports
(#%provide make-connector)
(#%provide has-value?)
(#%provide get-value)
(#%provide set-value!)
(#%provide forget-value!)
(#%provide connect)