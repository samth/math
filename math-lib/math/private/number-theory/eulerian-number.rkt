#lang typed/racket/base

(require typed/safe/ops
         "types.rkt")

(provide eulerian-number)

(: eulerian-number* : Natural Natural -> Natural)
;   computes the Eulerian number <n,k>
;   http://mathworld.wolfram.com/EulerianNumber.html
(define (eulerian-number* n k)
  ; Implementation note:        
  ;   Uses standard recurrence : <n,k> = (k+1) <n-1,k> + (n-k) <n-1,k-1>
  ;   Time: O(n^2)
  (cond
    [(= k 0) 1]
    [else
     (let ([m : (Refine [m : Positive-Integer] (>= m (+ 1 k)) (>= m (+ 1 n)))
              (max (+ k 1) (+ n 1))])
       ; <refined-local> Annotation added to E.
       (define E : (Refine [E : (Vectorof Integer)] (= m (len E))) (make-vector m 0))
       (safe-vector-set! E 0 1) ; <0,0> = 1
       ; <nope> It should be possible to add refinements to i and j, but I am unsure how.
       (let iloop ([i : (Refine [i : Positive-Integer] (<= i (+ n 1))) 1])
         (cond
           [(= i (+ n 1)) (void)]
           [else
            (let jloop ([j : (Refine [j : Integer] (<= 0 j) (< j i)) (- i 1)])
              (cond
                [(= j 0) (iloop (add1 i))]
                [else
                 (safe-vector-set! E j (+ (* (+ j 1) (safe-vector-ref E j))
                                          (* (- i j) (safe-vector-ref E (- j 1)))))
                 (jloop (sub1 j))]))]))
       (assert (safe-vector-ref E k) natural?))]))

(: eulerian-number (Integer Integer -> Natural))
(define (eulerian-number n k)
  (cond [(n . < . 0)  (raise-argument-error 'eulerian-number "Natural" 0 n k)]
        [(k . < . 0)  (raise-argument-error 'eulerian-number "Natural" 1 n k)]
        [else  (eulerian-number* n k)]))
