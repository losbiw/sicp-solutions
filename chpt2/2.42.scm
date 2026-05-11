; generic
(define nil '())

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

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

; solution

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? positions k))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
    
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position row column board)
  (if (= column 1)
    (list row)
    (append board (list row))))

(define (slope x1 x2 y1 y2)
  (/ (- x2 x1) (- y2 y1)))

(define (get-kth-element seq k)
  (define (iter items count)
    (if (= k count)
      (car items)
      (iter (cdr items) (+ count 1))))
  
  (iter seq 1))

(define (safe? positions column)
  (let ((row (get-kth-element positions column)))
    (define (iter rest current-column)
      (cond ((= column current-column) #t)
            ((and
              (not (= (car rest) row))
              (not (= (abs (slope current-column column (car rest) row)) 1)))
            (iter (cdr rest) (+ current-column 1)))
            (else #f)))

    (iter positions 1)))