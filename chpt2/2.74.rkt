#lang sicp

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (proc)
          (apply proc (map contents args))
          (error "no method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (get-record employee division-file)
  ((get 'get-record (type-tag division-file)) employee
                                              (contents division-file)))

; a) Each divisions file should be a cons cell, with car being the type tag and cdr being the contens of the file

(define (get-salary record)
  (apply-generic 'get-salary record))

; b) Each record should be a cons cell with the first element being the department tag.

(define (find-employee-record employee divisions)
  (if (null? divisions)
      (error "employee record does not exist -- FIND-EMPLOYEE-RECORD" employee)
      (let ((result (get-record employee (car divisions))))
        (if (null? result)
            (find-employee-record employee (cdr divisions))
            result))))

; d) implement their sets, tag with a corresponding tag for their division and expose an interface to its methods using put