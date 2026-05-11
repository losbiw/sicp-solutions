(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (fringe tree)
  (cond ((null? tree) ())
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) 5 (list 3 4 (list 6 7))))
