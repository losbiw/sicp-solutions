;generic

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; part a

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; part b

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
      (total-weight structure)
      structure)))

(define (total-weight mobile)
  (+
    (branch-weight (right-branch mobile))
    (branch-weight (left-branch mobile))))

; part c

(define (torque branch)
  (* (branch-weight branch) (branch-length branch)))

(define (balanced? structure)
  (if (not (pair? structure))
    #t
    (let ((left (left-branch structure)) (right (right-branch structure)))
        (and (= (torque left) (torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))

; part d
; just those

; (define (right-branch mobile)
;   (cdr mobile))

; (define (branch-structure branch)
;   (cdr branch))

; test
(define test-left (make-branch 5 12))
(define test-right (make-branch 6 7))
(define test-mobile (make-mobile test-left test-right))
