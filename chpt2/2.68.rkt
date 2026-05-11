#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(#%provide symbols)

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(#%provide encode)

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0
               (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1
               (encode-symbol symbol (right-branch tree))))
        (else (error "invalid symbol -- ENCODE-SYMBOL" symbol))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(A D A B B C A))