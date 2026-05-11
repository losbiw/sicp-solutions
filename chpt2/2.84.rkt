#lang sicp

; generic operations

(define (memq x set)
  (cond ((null? set) #f)
        ((eq? (car set)) set)
        (else (memq x (cdr set)))))

(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else (error "invalid datum"))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "invalid datum"))))

(define (attach-tag tag contents)
  (if (eq? tag 'integer)
      contents
      (cons tag contents)))

(define (apply-generic op . args)
  (let ((types (map type-tag args))
        (values (map contents args)))
    (let ((proc (get op types)))
      (cond (proc (apply proc values))
            ((same-type? args) (error "no function match for the argument types"))
            (else (apply apply-generic (cons op (raise-to-highest args))))))))

(define (same-type? args)
  (define (iter arg remaining)
    (cond ((null? remaining) #t)
          ((eq? (type-tag arg) (type-tag (car remaining)))
           (iter arg (cdr remaining)))
          (else #f)))

  (iter (car args) (cdr args)))

(define (raise-to-highest args)
  (let ((h (highest-type args)))
    (map (lambda (x) (raise-to x h type-tower)) args)))

(define (highest-type args)
  (define (iter current-highest remaining)
    (if (null? remaining)
        current-highest
        (let ((t2 (type-tag (car remaining))))
          (if (higher? current-highest t2 type-tower)
              (iter current-highest (cdr remaining))
              (iter t2 (cdr remaining))))))

  (if (pair? args)
      (iter (type-tag (car args)) (cdr args))
      (error "wrong list type")))

(define (higher? t1 t2 tower)
  (let ((m1 (memq t1 tower)))
    (if (null? (cdr m1))
        #t
        (not (memq t2 m1)))))

(define (raise-to x target)
  (if (eq? (type-tag x) (type-tag target))
      x
      (raise-to (apply-generic 'raise x) target)))

(define type-tower (list 'integer 'rational 'real 'complex))

; integers package

(define (install-integers-package)
  (define (tag x) (atttach-tag 'integer x))

  (put 'make 'integer (lambda (x) (tag x)))
  
  (put 'add 'integer (lambda (a b)
                       (tag (+ a b))))

  (put 'sub 'integer (lambda (a b)
                       (tag (- a b))))

  (put 'mul 'integer (lambda (a b)
                       (tag (* a b))))

  (put 'div 'integer (lambda (a b)
                       (tag (/ a b))))
  ; coercions
  (define (integer->rational i)
    ((get 'make 'rational) i 1))

  (put 'raise 'integer integer->rational)
  
  (put-coercion 'integer 'rational integer->rational))

; rationals package

(define (install-rational-package)  
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (numer x) (car x))
  
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ; interface

  (define (tag x) (attach-tag 'rational x))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  (put 'add 'rational (lambda (x y) (tag (add-rat x y))))

  (put 'sub 'rational (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul 'rational (lambda (x y) (tag (mul-rat x y))))

  (put 'div 'rational (lambda (x y) (tag (div-rat x y))))

  ; coercions
  (define (rational->real x)
    ((get 'make 'real) (/ (numer x) (denom x) 1.0)))

  (put 'raise 'rational rational->real)
  
  (put-coercion 'rational 'real rational->real))

; real package

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))

  (put 'make 'real (lambda (x) (tag x)))

  (put 'add 'integer (lambda (a b)
                       (tag (+ a b))))

  (put 'sub 'integer (lambda (a b)
                       (tag (- a b))))

  (put 'mul 'integer (lambda (a b)
                       (tag (* a b))))

  (put 'div 'integer (lambda (a b)
                       (tag (/ a b))))

  ; coercions

  (define (real->complex x)
    ((get 'make-from-real-imag 'complex) x 0))

  (put 'raise 'real real->complex)
  
  (put-coercion 'real 'complex real->complex))

; complex packages
(define (install-complex-package)
  (define (make-from-real-imag r i)
    ((get 'make-from-real-imag 'rectangular) r i))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'angular) a i))

  (define (real-part z) (apply-generic 'real-part z))

  (define (imag-part z) (apply-generic 'imag-part z))

  (define (magnitude z) (apply-generic 'magnitude z))

  (define (angle z) (apply-generic 'angle z))

  ; arithmetic

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ; interface
  (define (tag z) (attach-tag 'complex z))
  
  (put 'make-from-real-imag 'complex (lambda (r i)
                                       (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'complex (lambda (r a)
                                     (tag (make-from-mag-ang r a))))

  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'angle 'complex angle)

  (put 'add 'complex (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub 'complex (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul 'complex (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div 'complex (lambda (z1 z2) (tag (div-complex z1 z2)))))

;; rectangular form

(define (install-rectangular-complex-package)
  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag r i) (cons r i))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ; interface
  (define (tag z) (attach-tag 'rectangular z))

  (put 'make-from-real-imag 'rectangular (lambda (r i)
                                           (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'rectangular (lambda (r a)
                                         (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle))

;; angular form

(define (install-angular-complex-package)
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (magnitude z) (car z))

  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (make-from-real-imag r i)
    (cons (sqrt (+ (square r) (square i)))
          (aran r i)))

  ; interface

  (define (tag z) (attach-tag 'angular z))

  (put 'make-from-real-imag 'rectangular (lambda (r i)
                                           (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'rectangular (lambda (r a)
                                         (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle))

;; interfaces

(define (make-integer i)
  ((get 'make 'integer) i))

(define (make-rat x)
  ((get 'make 'rational) x))

(define (make-real r)
  ((get 'make 'real) r))

(define (make-complex-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (raise x) (apply-generic 'raise x))