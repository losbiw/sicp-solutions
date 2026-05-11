#lang sicp

(define (apply-specific op type . args)
  (let ((proc (get op type)))
    (if proc
        (apply proc args)
        (error "unknown type -- apply-specific" op type))))

(define (make-integer x)
  (apply-specific 'make 'integer x))

(define (make-rational numer denom)
  (apply-specific 'make 'rational numer denom))

(define (make-real x)
  (apply-specific 'make 'real x))

(define (make-complex-from-real-imag r i)
  (apply-specific 'make-from-real-imag 'complex r i))

; ----------

(define (integer->rational x)
  (make-rational (contents x) 1))

(put-coercion 'integer 'rational integer->rational)

(define (rational->real x)
  (make-real (/ (numer (contents x)) (denom (contents x)))))

(put-coercion 'rational 'real rational->real)

(define (real->complex x)
  (make-complex-from-real-imag (contents x) 0))

(put-coercion 'real 'complex rational->complex)

; -----------

(put 'raise '(integer) integer->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

(define (raise x)
  (apply-generic 'raise x))

