(define nil '())

(define (tree-map proc tree)
  (map (lambda (x)
          (if (pair? x)
            (tree-map proc x)
            (proc x)))
      tree))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))