#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-error.rkt"
         "expansion/expansion-base.rkt"
         "flvector.rkt")

(provide fllog1p fllog+
         lg1+ lg+ lg1- lg- lgsum
         fllog-quotient
         fllog2
         fllogb
         fllog-hypot)

(begin-encourage-inline
  
  (: fllog+ (Flonum Flonum -> Flonum))
  ;; Computes log(a+b) in a way that is accurate for a+b near 1.0
  ;; random testing shows 99.9% <0.5ulp and 100%< 1ulp
  (define (fllog+ a b)
    (define a+b (+ a b))
    (cond [(< 0.3 a+b 2.7)
           ;; a+b is too close to 1.0, so compute in higher precision
           (define-values (a+b a+b-lo) (fast-fl+/error a b))
           (- (fllog a+b) (fllog1p (- (/ a+b-lo a+b))))]
          [(a+b . = . +inf.0)
           ;; a+b overflowed, so reduce the arguments
           (+ (fllog 2.0) (fllog (+ (* 0.5 a) (* 0.5 b))))]
          [else
           (fllog a+b)]))
  
  (: lg1+ (Float -> Float))
  (define (lg1+ log-x)
    (cond [(log-x . fl>= . 0.0)  (fl+ log-x (fllog1p (flexp (- log-x))))]
          [else  (fllog1p (flexp log-x))]))
  
  (: lg+ (Float Float -> Float))
  (define (lg+ log-x log-y)
    (let ([log-x  (flmax log-x log-y)]
          [log-y  (flmin log-x log-y)])
      (cond [(fl= log-x -inf.0)  -inf.0]
            [else  (fl+ log-x (fllog1p (flexp (fl- log-y log-x))))])))
  
  (: lg1- (Float -> Float))
  (define (lg1- log-x)
    (cond [(log-x . fl> . (fllog 0.5))  (fllog (- (flexpm1 log-x)))]
          [else  (fllog1p (- (flexp log-x)))]))
  
  (: lg- (Float Float -> Float))
  (define (lg- log-x log-y)
    (cond [(log-x . fl< . log-y)  +nan.0]
          [(fl= log-x -inf.0)  -inf.0]
          [else  (fl+ log-x (lg1- (fl- log-y log-x)))]))
  
  )  ; begin-encourage-inline

(: flmax* ((Listof Flonum) -> Flonum))
(define (flmax* xs)
  (let loop ([xs xs] [mx -inf.0])
    (if (null? xs) mx (loop (cdr xs) (flmax mx (car xs))))))

(: lgsum ((Listof Flonum) -> Flonum))
(define (lgsum log-xs)
  (if (null? log-xs)
      0.0
      (let ([log-x0  (car log-xs)]
            [log-xs  (cdr log-xs)])
        (if (null? log-xs)
            log-x0
            (let ([log-x1  (car log-xs)]
                  [log-xs  (cdr log-xs)])
              (if (null? log-xs)
                  (lg+ log-x0 log-x1)
                  (let ([max-log-x  (flmax (flmax log-x0 log-x1) (flmax* log-xs))])
                    (if (fl= max-log-x -inf.0)
                        -inf.0
                        (let ([s  (flsum
                                   (list* -1.0  ; for the max element; faster than removing it
                                          (flexp (- log-x0 max-log-x))
                                          (flexp (- log-x1 max-log-x))
                                          (map (λ: ([log-x : Flonum]) (flexp (- log-x max-log-x)))
                                               log-xs)))])
                          ;; Yes, we subtract 1.0 and then add 1.0 before taking the log; this
                          ;; helps with precision a bit when s is near zero
                          (+ max-log-x (fllog1p s)))))))))))

(: fllog-quotient (Flonum Flonum -> Flonum))
;; Computes (fllog (/ x y)) in a way that reduces error, avoids under-/overflow
;; and is more accurate for x ≈ y.  random testing shows 96% <0.5 ulp 99.5%<1ulp
;; and 100% < 1.3 (when x very big and y very small or reversed)
(define (fllog-quotient x y)
  (let ([x  (flabs x)]
        [y  (flabs y)]
        [s  (fl/ (flsgn x) (flsgn y))])
    (cond [(s . fl> . 0.0)
           (define z (fl/ x y))
           (cond [(< 0.5 z 2.5)
                  (define-values (z z_) (fl//error x y))
                  (fl- (fllog z) (fllog1p (- (/ z_ z))))]
                 [(and (z . fl> . +max-subnormal.0) (z . fl< . +inf.0))  (fllog z)]
                 [else  (fl+ (fllog x) (- (fllog y)))])]
          [(and (s . fl= . 0.0) (not (flnan? x)))  -inf.0]
          [else  +nan.0])))

(: fllogb (Flonum Flonum -> Flonum))
;; Largest observed error is 2.1 ulps, but is usually < 0.7 ulps
(define (fllogb b x)
  (cond [(fl= x 1.0)  0.0]
        [(fl= b 1.0)
         ;; For x != 1, first limit wrt x: +inf.0 or -inf.0
         +nan.0]
        [(fl= b 2.0)
         ;; Using the more accurate `fllog2' ensures that exact cases have zero error
         (fllog2 x)]
        [(not (and (fl<= 0.0 b) (fl<= b +inf.0) (fl<= 0.0 x) (fl<= x +inf.0)))
         ;; One or both is out of bounds or is +nan.0
         +nan.0]
        [(fl= b 0.0)
         (cond [(fl= x 0.0)
                ;; First limit wrt x: +inf.0
                ;; First limit wrt b: 0.0
                ;; +inf.0 corrects left-inverse case (fllogb 0.0 (flexpt 0.0 +inf.0))
                ;; +inf.0 corrects right-inverse case (flexpt 0.0 (fllogb 0.0 0.0))
                +inf.0]
               [(fl= x +inf.0)
                ;; First limit wrt x: -inf.0
                ;; First limit wrt b: 0.0
                ;; -inf.0 corrects left-inverse case (fllogb 0.0 (flexpt 0.0 -inf.0))
                ;; -inf.0 corrects right-inverse case (flexpt 0.0 (fllogb 0.0 +inf.0))
                -inf.0]
               [(fl<= x 1.0)  0.0]
               [else  -0.0])]
        [(fl= b +inf.0)
         (cond [(fl= x 0.0)
                ;; First limit wrt x: -inf.0
                ;; First limit wrt b: -0.0
                ;; -inf.0 corrects left-inverse case (fllogb +inf.0 (flexpt +inf.0 -inf.0))
                ;; -inf.0 corrects right-inverse case (flexpt +inf.0 (fllogb +inf.0 0.0))
                -inf.0]
               [(fl= x +inf.0)
                ;; First limit wrt x: +inf.0
                ;; First limit wrt b: 0.0
                ;; +inf.0 corrects left-inverse case (fllogb +inf.0 (flexpt +inf.0 +inf.0))
                ;; +inf.0 corrects right-inverse case (flexpt +inf.0 (fllogb +inf.0 +inf.0))
                +inf.0]
               [(fl<= 1.0 x)  0.0]
               [else  -0.0])]
        [(fl= x 0.0)  (if (fl< b 1.0) +inf.0 -inf.0)]
        [(fl= x +inf.0)  (if (fl< b 1.0) -inf.0 +inf.0)]
        [else
         (define log-b (fllog b))
         (define y (fl/ (fllog x) log-b))
         ;; One Newton iteration reduces error to <= 1 ulp (instead of <= 2 ulps)
         (define numer (fl- x (flexpt b y)))
         (define denom (fl* x log-b))
         (cond [(and (numer . fl> . -inf.0) (numer . fl< . +inf.0)
                     (denom . fl> . 0.0) (denom . fl< . +inf.0))
                (fl+ y (fl/ numer denom))]
               [else
                ;; Oh noes! We had overflows or underflows!
                ;; Not a lot we can do without introducing more error, so just return y
                y])]))

(: fllog-hypot (-> Flonum Flonum Flonum))
;; computes (log (hypot x y)) avoiding over/underflow and more accurate when (hypot x y) ≈ 1.0
;; random testing shows 99.9% < 0.5ulp and 100% < 1ulp
(define (fllog-hypot X Y)
  (define x (flabs X))
  (define y (flabs Y))
  (define m (flhypot x y))
  (cond
    [(or (<= m .23e-307)
         (and (flinfinite? m) (flrational? x) (flrational? y)))
     (fl* 0.5 (lg+ (fl* 2. (fllog x)) (fl* 2. (fllog y))))]
    [(<= 0.15 m 2.7)
     (define-values (x² x²_) (fl*/error x x))
     (define-values (y² y²_) (fl*/error y y))
     (define-values (x+y x+y_) (fl2+ x² x²_ y² y²_))
     (fl* 0.5 (fl- (fllog x+y) (fllog1p (- (/ x+y_ x+y)))))]
    [(<= 0.025 m 25.)
     (fl* 0.5 (fllog (+ (flexpt x 2.) (flexpt y 2.))))]
    [else
     (fllog m)]))
