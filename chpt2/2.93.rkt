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

(define (first-of-type type set)
  (cond ((null? set) #f)
        ((eq? type (type-tag (car set))) (car set))
        (else (first-of-type type (cdr set)))))

(define (square x) (mul x x))

(define (same-variable? a b)
  (eq? a b))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

; generic types operations

(define (type-tag datum)
  (cond ((number? datum) 'integer)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "invalid datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((or (number? datum) (symbol? datum)) datum)
        ((pair? datum) (cdr datum))
        (else (error "invalid datum -- CONTENTS" datum))))

(define (attach-tag tag contents)
  (if (or (eq? tag 'integer) (eq? tag 'symbol))
      contents
      (cons tag contents)))

(define (apply-generic op . args)
  (let* ((types (map type-tag args))
         (values (map contents args))
         (first-polynomial (first-of-type 'polynomial args))
         (proc (get op types)))
      (cond (proc
             (let ((result (apply proc values)))
               (if (and (pair? result)
                        (in-tower? result)
                        (not (eq? op 'raise))
                        (not (eq? op 'project)))
                   (drop result)
                   result)))
            ((and (> (length args) 1) first-polynomial)
             (apply apply-generic
                    (cons op (map (lambda (x) (to-polynomial x (variable-g first-polynomial)))
                                  args))))
            ((and (or (not (in-tower? (car args)))
                      (tower-top? (car args)))
                  (same-type? args)) (error "No method for these types -- APPLY-GENERIC" (list op types)))
            ((same-type? args) (apply apply-generic (cons op (map raise args))))
            (else (apply apply-generic (cons op (raise-to-highest args)))))))

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
  (if (or (not (in-tower? x)) (tower-bottom? x))
      x
      (let ((projection (project x)))
        (if (not (in-tower? projection))
            x
            (let ((reraised (raise projection)))
              (if (equ? x reraised)
                  (drop projection)
                  x))))))

(define (tower-bottom? val)
  (eq? (type-tag val) (car type-tower)))

(define (tower-top? val)
  (define (find-last remaining)
    (if (null? (cdr remaining))
        (car remaining)
        (find-last (cdr remaining))))
  
  (eq? (type-tag val) (find-last type-tower)))

(define (in-tower? val)
  (memq (type-tag val) type-tower))

(define standalone-types '(polynomial))
(define type-tower (list 'integer 'rational 'real 'complex))

(define (to-polynomial x variable)
  (cond ((eq? (type-tag x) 'polynomial) x)
        ((in-tower? x) (make-dense-polynomial variable (list x)))
        (else (error "cannot convert to polynomial -- TO-POLYNOMIAL" x))))

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

(define (make-dense-polynomial var terms)
  ((get 'make-dense-polynomial 'polynomial) var terms))

(define (make-sparse-polynomial var terms)
  ((get 'make-sparse-polynomial 'polynomial) var terms))

(define (make-term order coeff)
  ((get 'make-term 'polynomial) order coeff))

(define (order term)
  ((get 'order 'polynomial) term))
(define (coeff term)
  ((get 'coeff 'polynomial) term))
(define (variable-g polynomial)
  (apply-generic 'variable polynomial))
(define (term-list-g polynomial)
  (apply-generic 'term-list polynomial))
(define (coerce-polynomial polynomial variable)
  (apply-generic 'coerce polynomial variable))

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
(define (=zero? x) (apply-generic '=zero? x))
(define (negate x) (apply-generic 'negate x))
(define (quotient-g x y) (apply-generic 'quotient x y))
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))

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

  (put =zero? '(integer) zero?)
  (put 'quotient '(integer integer) quotient)
  (put 'gcd '(integer integer) gcd)
  
  ; coercions
  (define (integer->rational i)
    ((get 'make 'rational) i 1))

  (put 'raise '(integer) integer->rational))

; rationals package

(define (install-rational-package)  
  (define (make-rat n d)
    (let ((g (greatest-common-divisor n d)))
      (cons (div n g) (div d g))))

  (define (numer x) (car x))
  
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (equ-rat? a b)
    (and (equ? (numer a) (numer b))
         (equ? (denom a) (denom b))))

  (define (=zero-rat? rat)
    (equ? (numer rat) 0))

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

  (put 'equ? '(rational rational) equ-rat?)

  (put '=zero? '(rational) =zero-rat?)

  ; coercions
  (define (rational->real x)
    ((get 'make 'real) (exact->inexact (/ (numer x) (denom x)))))

  (define (rational->integer x)
    ((get 'make 'integer) (quotient-g (numer x) (denom x))))

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
  (put 'negate '(real) (lambda (x) (tag (- x))))

  (put 'equ? '(real real)
       (lambda (a b) (= a b)))

  (put '=zero? '(real) zero?)

  (put 'quotient '(real real) quotient)
  (put 'gcd '(real real) gcd)

  ; coercions

  (define (real->complex x)
    ((get 'make-from-real-imag 'complex) (make-real x) (make-real 0)))

  (define (real->polynomial r poly)
    (make-dense-polynomial (variable-g poly) (list r)))

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

  (define (=zero-complex? z)
    (equ? (magnitude z) 0))

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

  (put '=zero? '(complex) =zero-complex?)

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

; polynomial package
(define (install-polynomial-package)
  (define (make-polynomial var term-list)
    (cons var (apply-generic 'make term-list)))
  (define (make-dense-polynomial var term-list)
    (cons var ((get 'make '(dense-poly)) term-list)))
  (define (make-sparse-polynomial var term-list)
    (cons var ((get 'make '(sparse-poly)) term-list)))
  (define (make-term order coeff)
    (list order coeff))

  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (the-empty-termlist type) (get 'the-empty-termlist type))
  (define (the-empty-termlist-sparse) (get 'the-empty-termlist 'sparse-poly))
  (define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
  (define (first-term term-list) (apply-generic 'first-term term-list))
  (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
  (define (adjoin-term term term-list) ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))

  (define (add-polynomials p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (add-terms (term-list p1)
                                    (term-list p2)))
        (error "polynomials not in same var -- ADD-POLYNOMIALS" (list p1 p2))))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1))
                 (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms l1 (rest-terms l2))))
                   (else (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms l1)
                                                 (rest-terms l2)))))))))

  (define (sub-polynomials p1 p2)
    (add-polynomials p1 (negate-poly p2)))

  (define (negate-poly p)
    (make-polynomial (variable p)
                     (negate-terms (term-list p))))

  (define (negate-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist (type-tag term-list))
        (let ((term (first-term term-list)))
          (let ((new-term (make-term (order term) (negate (coeff term)))))
            (adjoin-term new-term
                         (negate-terms (rest-terms term-list)))))))
  
  (define (mul-polynomials p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (mul-terms (term-list p1)
                                    (term-list p2)))
        (error "polynomials not in same var -- MUL-POLYNOMIALS" (list p1 p2))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist-sparse)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (quotient result-list) (car result-list))
  (define (remainder result-list) (cadr result-list))
  (define (quotient-terms p1 p2) (quotient (div-polynomials p1 p2)))
  
  (define (div-polynomials p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div-terms (term-list p1) (term-list p2))))
          (list (make-polynomial (variable p1)
                                 (quotient div-result))
                (make-polynomial (variable p1)
                                 (remainder div-result))))
        (error "polys not in same var -- DIV-POLYNOMIALS" (list p1 p2))))

  (define (div-terms l1 l2)
    (if (empty-termlist? l1)
        (list (the-empty-termlist-sparse) (the-empty-termlist-sparse))
        (let ((t1 (first-term l1))
              (t2 (first-term l2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist-sparse) l1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (sub (order t1) (order t2))))
                (let ((quotient-term (make-term new-o new-c)))
                  (let ((multiplied (mul-term-by-all-terms quotient-term l2)))
                      (let ((rest-of-result (div-terms
                                             (add-terms l1 (negate-terms multiplied))
                                             l2)))
                        (list (adjoin-term quotient-term
                                           (quotient rest-of-result))
                              (remainder rest-of-result))))))))))

  (define (remainder-terms l1 l2) (remainder (div-terms l1 l2)))

  (define (mul-term-by-all-terms t1 term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist-sparse)
        (let ((t2 (first-term term-list)))
          (adjoin-term (make-term (add (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms term-list))))))

  (define (gcd-terms l1 l2)
    (if (empty-termlist? l2)
        l1
        (gcd-terms l2 (remainder-terms l1 l2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-polynomial (variable p1)
                         (gcd-terms (term-list p1)
                                    (term-list p2)))
        (error "Polys not in same variable -- GCD-POLY" (list p1 p2))))

  (define (=zero-poly? p)
    (define (all-zero? terms)
      (or (empty-termlist? terms)
          (and (=zero? (coeff (first-term terms)))
               (all-zero? (rest-terms terms)))))

    (all-zero? (term-list p)))

  (define (poly? x)
    (and (pair? x) (eq? (car x) 'polynomial)))

  (define (scale-termlist tl factor)
    (if (empty-termlist? tl)
        (the-empty-termlist (type-tag tl))
        (let ((ft (first-term tl)))
          (adjoin-term (make-term (order ft)
                                  (mul (coeff ft) factor))
                       (scale-termlist (rest-terms tl) factor)))))

  (define (coerce-polynomial-local poly target-variable)
    (let ((o-variable (variable poly))
          (o-termlist (term-list poly)))
  
      (define (coerce-termlist tl)
        (if (empty-termlist? tl)
            (the-empty-termlist-sparse)
            (let* ((term (first-term tl))
                   (nested-poly (coeff term)))
              (cond ((and (poly? nested-poly)
                          (eq? target-variable (variable-g nested-poly)))
                     (let* ((coercion-factor (tag (make-sparse-polynomial o-variable
                                                                          (list (make-term (order term) 1)))))
                            (coerced-termlist (scale-termlist (term-list-g nested-poly)
                                                              coercion-factor)))
                       (add-terms coerced-termlist
                                  (coerce-termlist (rest-terms tl)))))
                    ((poly? nested-poly) (coerce-polynomial (contents nested-poly) target-variable))
                    (else (error "target variable not in polynomial -- COERCE-POLY" (list poly target-variable)))))))

      (make-polynomial target-variable
                       (coerce-termlist o-termlist))))

  ;; interface

  (define (tag p) (attach-tag 'polynomial p))
  (put 'make-dense-polynomial 'polynomial
       (lambda (v t) (tag (make-dense-polynomial v t))))
  (put 'make-sparse-polynomial 'polynomial
       (lambda (v t) (tag (make-sparse-polynomial v t))))
  (put 'make-term 'polynomial make-term)
  (put 'order 'polynomial order)
  (put 'coeff 'polynomial coeff)
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-polynomials p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-polynomials p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-polynomials p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((div-result (div-polynomials p1 p2)))
           (list (tag (quotient div-result))
                 (tag (remainder div-result))))))
  (put 'quotient '(polynomial polynomial) (lambda (p1 p2) (tag (quotient-terms p1 p2))))
  (put 'coerce '(polynomial symbol) (lambda (poly var) (tag (coerce-polynomial-local poly var))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'negate '(polynomial) negate-poly))

; dense polynomial package

(define (install-dense-polynomial-package)
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list) (cdr term-list))

  (define (adjoin-term term term-list)
    (define (fill-zeroes order highest-order)
      (if (eq? order highest-order)
          term-list
          (cons 0 (fill-zeroes (sub order 1)
                               highest-order))))

    (if (=zero? (coeff term))
        term-list
        (cons (coeff term) (fill-zeroes (sub (order term) 1)
                                        (sub (length term-list) 1)))))

  (define (empty-termlist? term-list) (null? term-list))

  ;; interface
  (define (tag p) (attach-tag 'dense-poly p))

  (put 'make '(dense-poly)
       (lambda (term-list) (tag term-list)))
  (put 'first-term '(dense-poly) first-term)
  (put 'rest-terms '(dense-poly)
       (lambda (p) (tag (rest-terms p))))
  (put 'adjoin-term 'dense-poly
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'empty-termlist? '(dense-poly) empty-termlist?)
  (put 'the-empty-termlist 'dense-poly (tag '())))

;; sparse polynomial representation

(define (install-sparse-polynomial-package)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (empty-termlist? term-list) (null? term-list))

  ;; interface
  (define (tag p) (attach-tag 'sparse-poly p))

  (put 'make '(sparse-poly)
       (lambda (term-list) (tag term-list)))
  (put 'first-term '(sparse-poly) first-term)
  (put 'rest-terms '(sparse-poly)
       (lambda (p) (tag (rest-terms p))))
  (put 'adjoin-term 'sparse-poly
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'empty-termlist? '(sparse-poly) empty-termlist?)
  (put 'the-empty-termlist 'sparse-poly (tag '())))

;; installations

(install-integers-package)
(install-rational-package)
(install-real-package)
(install-rectangular-complex-package)
(install-angular-complex-package)
(install-complex-package)
(install-dense-polynomial-package)
(install-sparse-polynomial-package)
(install-polynomial-package)

;; test
(define p1 (make-sparse-polynomial 'x '((2 1)(0 1))))
(define p2 (make-sparse-polynomial 'x '((3 1)(0 1))))
(define rf (make-rat p2 p1))
(add rf rf)