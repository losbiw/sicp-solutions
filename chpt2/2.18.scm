(define (reverse lst)
  (define (iter a result)
    (if (null? a)
      result
      (iter (cdr a) 
            (cons (car a) result))))
  
  (iter lst ()))