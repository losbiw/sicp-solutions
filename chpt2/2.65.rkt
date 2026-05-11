#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))

  (copy-to-list tree nil))

(define (list->tree elts)
  (car (partial-tree elts (length elts))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts ))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (ordered-lists-intersection set1 set2)
  (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((= x1 x2) (cons x1
                                 (ordered-lists-intersection (cdr set1)
                                                             (cdr set2))))
                ((> x1 x2) (ordered-lists-intersection set1 (cdr set2)))
                (else (ordered-lists-intersection (cdr set1) set2))))))

(define (ordered-lists-union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (ordered-lists-union (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (ordered-lists-union (cdr set1) set2)))
                 (else (cons x2 (ordered-lists-union set1 (cdr set2)))))))))

(define (intersection-set set1 set2)
  (list->tree (ordered-lists-intersection (tree->list set1)
                                          (tree->list set2))))

(define (union-set set1 set2)
  (list->tree (ordered-lists-union (tree->list set1)
                                   (tree->list set2))))


(define a-tree (make-tree 8
                          (make-tree 3
                                     (make-tree 1 nil nil)
                                     (make-tree 5 nil nil))
                          (make-tree 9
                                     nil
                                     (make-tree 11 nil nil))))

(define b-tree (make-tree 3
                          (make-tree 1 nil nil)
                          (make-tree 7
                                     (make-tree 5 nil nil)
                                     (make-tree 9
                                                nil
                                                (make-tree 11 nil nil)))))