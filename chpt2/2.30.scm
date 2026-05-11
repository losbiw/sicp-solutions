(define nil '())

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (* tree tree))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))

(define (square-tree-map tree)
  (map (lambda (x)
          (if (pair? x)
            (square-tree x)
            (* x x)))
        tree))