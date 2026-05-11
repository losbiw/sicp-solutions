(define nil '())

(define (enumerate-interval l h)
  (if (> l h)
    nil
    (cons l (enumerate-interval (+ l 1) h))))

(define (map proc seq)
  (if (null? seq)
    nil
    (cons (proc (car seq)) (map proc (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op initial (cdr seq)))))

(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (x)
      (map 
        (lambda (y) (list x y))
        (enumerate-interval 1 (- x 1))))
    (enumerate-interval 1 n)))

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))