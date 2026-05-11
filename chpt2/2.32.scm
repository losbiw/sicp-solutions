(define nil '())

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (append lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))