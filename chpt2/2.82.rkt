#lang sicp

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad typed datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad typed datum -- CONTENTS" datum))))

(define (attach-tag object tag)
  (if (eq? tag 'scheme-number)
      object
      (cons object tag)))

(define (coerce-multiple target args)
  (define (iter args result)
    (let ((type (type-tag (car args)))
          (arg-val (contents (car args))))
      (cond ((null? args) result)
            ((eq? target type) (iter (cdr args) (append result
                                                        arg-val)))
            (else (let ((coercion (get-coercion target
                                                (type-tag (car args)))))
                    (if coercion
                        (iter (cdr args) (append result (coercion arg-val)))
                        (else #f)))))))

  (iter args nil))

(define (coerce-all args)
  (define (iter remaining-args)
    (if (null? remaining-args)
      #f
      (or (coerce-multiple (car remaining-args)
                           args)
          (iter (cdr remaining-args)))))

  (iter args))

(define (apply-generic op . args)
  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (coerce-all args)))
            (if coerced-args
                (apply apply-generic (cons op coerced-args))
                (error "No generic procedure for these types -- APPLY-GENERIC" op types)))))))

; The strategy is not sufficiently generic when none of the arguments are of type that is required by the procedure.
; E.g., 2 or more scheme-numbers being passing into exp procedure defined for complex numbers.
; Even though an appropriate coercion mechanism (scheme-number->complex)