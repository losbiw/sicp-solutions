#lang sicp

(define (make-table same-key?)
  (define (empty-table) (list '*table*))
  (define (records table) (cdr table))

  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (empty-table)))
    (define (lookup keys)
      (if (null? keys)
          (cdr local-table)
          (let* ((recs (records local-table))
                 (key (car keys))
                 (entry (assoc key recs)))
            (if (and entry (table? (cdr entry)))
                (table-lookup (cdr entry) (cdr keys))
                false))))

    (define (insert! keys value)
      (if (null? keys)
          (set-cdr! local-table value)
          (let* ((recs (records local-table))
                 (key (car keys))
                 (entry (assoc key recs)))
            (if entry
                (let ((entry-val (cdr entry)))
                  (if (table? entry-val)
                      (table-insert! entry-val (cdr keys) value)
                      (begin
                        (set-cdr! entry (make-table eq?))
                        (table-insert! (cdr entry) (cdr keys) value))))
                (let ((new-subtable (make-table eq?)))
                  (set-cdr! local-table
                            (cons (cons key new-subtable) recs))
                  (table-insert! new-subtable (cdr keys) value))))))

    (define (print)
      (newline)
      (display (list 'print local-table))
      local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown Operator -- MAKE-TABLE" m))))

    (cons 'compound-table dispatch)))

(define (table? object)
  (and (pair? object) (eq? (car object) 'compound-table)))

(define (table-insert! table keys value)
  (((cdr table) 'insert-proc) keys value))

(define (table-lookup table keys)
  (((cdr table) 'lookup-proc) keys))

(define (lookup-and-print table keys)
  (newline)
  (display (table-lookup table keys)))

(define test-table (make-table equal?))

(table-insert! test-table  '(a b) 1)
(lookup-and-print test-table '(a b))
(table-insert! test-table '(a b c d) 4)
(lookup-and-print test-table '(a b c d))
(table-insert! test-table '(a b) 2)
(lookup-and-print test-table '(a b))