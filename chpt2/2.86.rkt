#lang sicp

; get/put

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

; generic operations

(define (memq x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) set)
        (else (memq x (cdr set)))))

(define (square x) (mul x x))

; generic types operations

(define (type-tag datum)
  (cond ((number? datum) 'integer)
        ((pair? datum) (car datum))
        (else (error "invalid datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "invalid datum -- CONTENTS" datum))))

(define (attach-tag tag contents)
  (if (eq? tag 'integer)
      contents
      (cons tag contents)))

(define (apply-generic op . args)
  (let ((types (map type-tag args))
        (values (map contents args)))
    (let ((proc (get op types)))
      (newline)
      (display args)
      (cond (proc
             (let ((result (apply proc values)))
               (if (and (pair? result)
                        (not (or (eq? op 'raise)
                                 (eq? op 'project))))
                   (drop result)
                   result)))
            ((and (tower-top? (car args)) (same-type? args)) (error "No method for these types -- APPLY-GENERIC" (list op types)))
            ((same-type? args) (apply apply-generic (cons op (map raise args))))
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
    (map (lambda (x) (raise-to x h)) args)))

(define (highest-type args)
  (define (iter current-highest remaining)
    (if (null? remaining)
        current-highest
        (let ((t2 (type-tag (car remaining))))
          (if (higher? current-highest t2)
              (iter current-highest (cdr remaining))
              (iter t2 (cdr remaining))))))

  (if (pair? args)
      (iter (type-tag (car args)) (cdr args))
      (error "Not a list -- HIGHEST-TYPE" args)))

(define (higher? t1 t2)
  (let ((m1 (memq t1 type-tower)))
    (if (null? (cdr m1))
        #t
        (not (memq t2 m1)))))

(define (raise-to x target)
  (if (eq? (type-tag x) target)
      x
      (raise-to (raise x)
                target)))

(define (drop x)
  (if (tower-bottom? x)
      x
      (let ((projection (apply-generic 'project x)))
        (let ((reraised (apply-generic 'raise projection)))
          (if (equ? x reraised)
              (drop projection)
              x)))))

(define (tower-bottom? val)
  (eq? (type-tag val) (car type-tower)))

(define (tower-top? val)
  (define (find-last remaining)
    (if (null? (cdr remaining))
        (car remaining)
        (find-last (cdr remaining))))
  
  (eq? (type-tag val) (find-last type-tower)))

(define type-tower (list 'integer 'rational 'real 'complex))

; interfaces

(define (make-integer i)
  ((get 'make 'integer) i))

(define (make-rat n d)
  ((get 'make 'rational) n d))

(define (make-real r)
  ((get 'make 'real) r))

(define (make-complex-from-real-imag r i)
  ((get 'make-from-real-imag 'complex) r i))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add a b) (apply-generic 'add a b))
(define (sub a b) (apply-generic 'sub a b))
(define (mul a b) (apply-generic 'mul a b))
(define (div a b) (apply-generic 'div a b))
(define (equ? a b) (apply-generic 'equ? a b))
(define (sqrt-g x) (apply-generic 'sqrt-g x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atan-g x y) (apply-generic 'atan-g x y))

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

; integers package

(define (install-integers-package)
  (define (tag x) (attach-tag 'integer x))

  (put 'make 'integer
       (lambda (x) (tag x)))
  
  (put 'add '(integer integer)
       (lambda (a b) (tag (+ a b))))

  (put 'sub '(integer integer)
       (lambda (a b) (tag (- a b))))

  (put 'mul '(integer integer)
       (lambda (a b) (tag (* a b))))

  (put 'div '(integer integer)
       (lambda (a b) (tag (/ a b))))

  (put 'equ? '(integer integer)
       (lambda (a b) (= a b)))
  
  ; coercions
  (define (integer->rational i)
    ((get 'make 'rational) i 1))

  (put 'raise '(integer) integer->rational))

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

  (define (equ? a b)
    (and (= (numer a) (numer b))
         (= (denom a) (denom b))))

  ; interface

  (define (tag x) (attach-tag 'rational x))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational) equ?)

  ; coercions
  (define (rational->real x)
    ((get 'make 'real) (exact->inexact (/ (numer x) (denom x)))))

  (define (rational->integer x)
    ((get 'make 'integer) (quotient (numer x) (denom x))))

  (put 'project '(rational) rational->integer)
  (put 'raise '(rational) rational->real))

; real package

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))

  (put 'make 'real
       (lambda (x) (tag x)))

  (put 'add '(real real)
       (lambda (a b) (tag (+ a b))))

  (put 'sub '(real real)
       (lambda (a b) (tag (- a b))))

  (put 'mul '(real real)
       (lambda (a b) (tag (* a b))))

  (put 'div '(real real)
       (lambda (a b) (tag (/ a b 1.0))))

  (put 'sqrt-g '(real) (lambda (x) (tag (sqrt x))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'atan-g '(real real) (lambda (x y) (tag (atan x y))))

  (put 'equ? '(real real)
       (lambda (a b) (= a b)))

  ; coercions

  (define (real->complex x)
    ((get 'make-from-real-imag 'complex) (make-real x) (make-real 0)))

  (define (real->rational x)
    (let ((y (inexact->exact x)))
      ((get 'make 'rational) (numerator y) (denominator y))))

  (put 'raise '(real) real->complex)
  (put 'project '(real) real->rational))

; complex packages
(define (install-complex-package)
  (define (make-from-real-imag r i)
    ((get 'make-from-real-imag 'rectangular) r i))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'angular) r a))

  ; arithmetic

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (equ-complex? z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))

  ; interface
  (define (tag z) (attach-tag 'complex z))
  
  (put 'make-from-real-imag 'complex
       (lambda (r i) (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  
  (put 'equ? '(complex complex) equ-complex?)

  ;coercions
  (define (complex->real z)
    (real-part z))

  (put 'project '(complex) complex->real))

;; rectangular form

(define (install-rectangular-complex-package)
  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt-g (add (square (real-part z))
                 (square (imag-part z)))))

  (define (angle z)
    (atan-g (imag-part z) (real-part z)))

  (define (make-from-real-imag r i) (cons r i))

  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  ; interface
  (define (tag z) (attach-tag 'rectangular z))

  (put 'make-from-real-imag 'rectangular
       (lambda (r i) (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle))

;; angular form

(define (install-angular-complex-package)
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))

  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))

  (define (magnitude z) (car z))

  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (make-from-real-imag r i)
    (cons (sqrt-g (add (square r) (square i)))
          (atan-g r i)))

  ; interface

  (define (tag z) (attach-tag 'angular z))

  (put 'make-from-real-imag 'angular
       (lambda (r i) (tag (make-from-real-imag r i))))

  (put 'make-from-mag-ang 'angular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(angular) real-part)
  (put 'imag-part '(angular) imag-part)
  (put 'magnitude '(angular) magnitude)
  (put 'angle '(angular) angle))

;; installations

(install-integers-package)
(install-rational-package)
(install-real-package)
(install-rectangular-complex-package)
(install-angular-complex-package)
(install-complex-package)

; test

(define test-unit (make-complex-from-real-imag (make-real 3.5) (make-real 0)))