#lang sicp

(define (square x) (* x x))

(define (rand-update x) (random (expt 2 31)))
(define random-init (rand-update 0))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))

  (iter trials 0))

(define (area-from-opp-corners x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (estimate-integral pred trials x1 x2 y1 y2)
  (* (area-from-opp-corners x1 x2 y1 y2)
     (monte-carlo trials
                  (lambda ()
                    (pred (random-in-range x1 x2)
                          (random-in-range y1 y2))))))

(define (circle-predicate x-center y-center radius)
  (lambda (x y)
    (<= (+ (square (- x x-center))
           (square (- y y-center)))
        (square radius))))

(define (estimate-pi trials)
  (define unit-circle-pred (circle-predicate 0 0 1))
  
  (estimate-integral unit-circle-pred
                     trials
                     -1.0 1.0 -1.0 1.0))

(estimate-pi 100000)