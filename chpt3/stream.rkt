#lang sicp

;; (define (cons-stream a b)
;;   (cons a (delay b)))
;; 
(define (stream-car stream)
  (car stream))
 
(define (stream-cdr stream)
  (force (cdr stream)))
;; 
;; (define stream-null? null?)
;; 
;; (define the-empty-stream '())

; misc

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s)
                  (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc
                              (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-n-stream s n)
  (if (= n 0)
      'done
      (begin
        (display-line (stream-car s))
        (display-n-stream (stream-cdr s) (- n 1)))))

(define (display-line x)
  (newline)
  (display x))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (negate-stream s)
  (stream-map (lambda (x) (- x))
              s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones
                                             integers)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; exports

(#%provide cons-stream)
(#%provide stream-car)
(#%provide stream-cdr)
(#%provide stream-null?)
(#%provide the-empty-stream)
(#%provide stream-ref)
(#%provide stream-map)
(#%provide stream-for-each)
(#%provide stream-enumerate-interval)
(#%provide stream-filter)
(#%provide display-stream)
(#%provide display-n-stream)
(#%provide display-line)
(#%provide scale-stream)
(#%provide negate-stream)
(#%provide add-streams)
(#%provide mul-streams)
(#%provide partial-sums)
(#%provide ones)
(#%provide integers)
(#%provide integers-starting-from)