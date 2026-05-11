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

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; solution

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge (adjoin-set (make-code-tree (car leaves)
                                                    (cadr leaves))
                                    (cddr leaves)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(#%provide generate-huffman-tree)

; test
; (define test-tree (generate-huffman-tree '(('A 8) ('B 3) ('C 1) ('D 1) ('E 1) ('G 1) ('H 1))))