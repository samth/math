#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-log.rkt"
         "flonum-error.rkt"
         "flvector.rkt"
         "utils.rkt")

(provide flsqrt1pm1
         flsinh flcosh fltanh
         flasinh flacosh flatanh
         make-flexpt flexpt+ flexpt1p)

;; ---------------------------------------------------------------------------------------------------
;; sqrt(1+x)-1

(: flsqrt1pm1 (Float -> Float))
(define (flsqrt1pm1 x)
  (cond [((flabs x) . fl> . 0.75)
         (fl- (flsqrt (fl+ 1.0 x)) 1.0)]
        [else
         (flexpm1 (fl* 0.5 (fllog1p x)))]))

;; ---------------------------------------------------------------------------------------------------
;; Exponential with high-precision bases

(begin-encourage-inline
  
  (: make-flexpt (Positive-Exact-Rational -> (Flonum -> Flonum)))
  (define (make-flexpt b)
    (define b-hi (fl b))
    (define b-lo (fl (- (/ (inexact->exact b-hi) b) 1)))
    (cond [(fl= b-lo 0.0)  (λ: ([x : Flonum]) (flexpt b-hi x))]
          [else
           (λ: ([x : Flonum])
             (fl/ (flexpt b-hi x)
                  (flexp (fl* x (fllog1p b-lo)))))]))

  (: flexpt+ (Flonum Flonum Flonum -> Flonum))
  (define (flexpt+ a b y)
    (define-values (x-hi x-lo) (fast-fl+/error a b))
    (fl/ (flexpt x-hi y)
         (flexp (fl* y (fllog1p (- (/ x-lo x-hi)))))))
  
  (: flexpt1p (Flonum Flonum -> Flonum))
  (define (flexpt1p x y)
    (cond [(and (x . > . -0.5) (x . < . +inf.0))
           (define-values (a-hi a-lo) (fast-fl+/error 1.0 x))
           (fl/ (flexpt a-hi y)
                (flexp (fl* y (fllog1p (- (/ a-lo a-hi))))))]
          [else  (flexpt (+ 1.0 x) y)]))
  
  )  ; begin-encourage-inline
