(define nil '())

(define (enumerate-interval l h)
  (if (> l h)
    nil
    (cons l (enumerate-interval (+ l 1) h))))

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

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; end of boilerplate

(define (unique-triples n)
  (flatmap (lambda (x)
        (flatmap (lambda (y)
              (map (lambda (z) (list x y z)) (enumerate-interval 1 (- y 1))))
              (enumerate-interval 1 (- x 1))))
        (enumerate-interval 1 n)))

(define (sums-to-target? target triple)
  (= target (+ (car triple) (cadr triple) (caddr triple))))

(define (target-sum-triples n target)
  (filter (lambda (triple) (sums-to-target? target triple))
    (unique-triples n)))