#lang sicp

(define (lookup lookup-key records-tree)
  (if (null? records-tree)
      false
      (let ((this-record (entry records-tree)))
        (let ((this-key (key this-record)))
          (cond ((= lookup-key this-key) this-record)
                ((< lookup-key this-key) (lookup lookup-key
                                                 (left-tree records-tree)))
                (else (lookup lookup-key (right-tree records-tree))))))))