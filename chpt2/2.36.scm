(define nil '())

(define (accumulate op initial items)
  (if (null? items)
    initial
    (op (car items) (accumulate op initial (cdr items)))))

(define (map proc items)
  (accumulate (lambda (x y) (cons (proc x) y))
              nil
              items))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))