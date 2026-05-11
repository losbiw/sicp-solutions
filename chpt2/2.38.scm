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

; 1) 3/2
; 2) 1/6
; 3) (1 (2 (3 ())))
; 4) (((() 1) 2) 3)