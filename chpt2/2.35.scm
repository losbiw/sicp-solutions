(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map proc items)
  (accumulate (lambda (x y) (cons (proc x) y))
              nil
              items))

; (define (count-leaves t)
;   (accumulate (lambda (x y) (+ (if (pair? x) (count-leaves x) 1) y))
;               0
;               t))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (subtree) (if (pair? subtree) (count-leaves subtree) 1)) t)))