(define nil '())

(define (fold-right op initial items)
  (if (null? items)
    initial
    (op (car items) (fold-right op initial (cdr items)))))

(define (fold-left op initial items)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
    
  (iter initial items))

(define (append lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

; answer

(define (reverse-right seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))