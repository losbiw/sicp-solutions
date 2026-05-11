(define (deep-reverse l)
  (define (iter items res)
    (cond ((null? items) res)
          ((pair? (car items))
            (iter (cdr items) 
                  (cons (deep-reverse (car items)) res)))
          (else (iter (cdr items) 
                      (cons (car items) res)))))
          
  (iter l ()))

(define x (list (list 1 2) (list 3 4)))