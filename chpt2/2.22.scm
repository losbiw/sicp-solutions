(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (push answer (square (car things))))))

  (iter items ()))

(define (push lst item)
  (if (null? lst)
    (cons item ())
    (cons (car lst) (push (cdr lst) item))))

(define (square x) (* x x))

; 1st version: existing answer is being appended to the end of the pair.
;   i.e, when car things = 2, items = (1 2 3 4) - answer = (2 (1 ()))

; 2nd version: List starts with the null terminator. Nesting happens the other way around
;   i.e the list expands from right to left instead of left to right

; Pretty sure it'd be possible with the append procedure, however that would turn it from O(n) to O(n^2) procedure