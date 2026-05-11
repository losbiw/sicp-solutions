(define (for-each proc items)
  (unless (null? items)
    (proc (car items))
    (for-each proc (cdr items))))
