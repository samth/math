#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt")

(provide flexpm1 flexpsqr flgauss flexp1p flexp2 flpow2near)

(begin-encourage-inline
  
  (: flgauss (Flonum -> Flonum))
  ;; Computes exp(-x^2) in a way that is accurate for large x
  (define (flgauss x)
    (let ([x  (flabs x)])
      (cond [(x . fl> . 28.0)  0.0]
            [(x . fl> . 1.0)
             ;; Split x into a flonum with 21 high-order fractional bits and a part with the rest
             ;; Sometime after p > 26.1, (exp (- (* p p))) outputs subnormals, so we don't go there
             (define p (flmin 26.0 (fl/ (fltruncate (fl* (flexpt 2.0 21.0) x)) (flexpt 2.0 21.0))))
             (define q (fl- x p))
             (fl* (fl* (flexp (- (fl* 2.0 (fl* p q))))
                       (flexp (- (fl* q q))))
                  (flexp (- (fl* p p))))]
            [else
             (flexp (- (fl* x x)))])))
  
  (: flexpsqr (Flonum -> Flonum))
  ;; Computes exp(x^2) in a way that is accurate for large x
  (define (flexpsqr x)
    (let ([x  (flabs x)])
      (cond [(x . fl> . 27.0)  +inf.0]
            [(x . fl> . 1.0)
             (define p (fl/ (fltruncate (fl* (flexpt 2.0 21.0) x)) (flexpt 2.0 21.0)))
             (define q (fl- x p))
             (fl* (fl* (flexp (fl* 2.0 (fl* p q)))
                       (flexp (fl* q q)))
                  (flexp (fl* p p)))]
            [else
             (flexp (fl* x x))])))
  
  (: flexp1p (Flonum -> Flonum))
  ;; Computes exp(1+x) in a way that is accurate near powers of 2
  (define (flexp1p x)
    (cond [(x . fl< . -0.5)  (flexp (fl+ 1.0 x))]
          [else
           (define lg2x (flfloor (fl/ (fllog x) (fllog 2.0))))
           (define lg2x+1 (flfloor (fl/ (fllog (fl+ 1.0 x)) (fllog 2.0))))
           (cond [(fl= lg2x lg2x+1)  (flexp (fl+ 1.0 x))]
                 [else  (fl* (flexp x) (flexp 1.0))])]))
  
  (: flpow2near (-> Flonum Flonum))
  (define (flpow2near a)
    (flexp2 (flmax -1074.0 (flmin 1023.0 (flround (fl/ (fllog (flabs a)) (fllog 2.0)))))))
  
  )  ; begin-encourage-inline
