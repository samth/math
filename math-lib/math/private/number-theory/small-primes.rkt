#lang typed/racket

(require typed/safe/ops)
(require/typed
 data/bit-vector               
 [#:opaque BitVector bit-vector?]
 [make-bit-vector (Integer Boolean -> BitVector)]
 [bit-vector-set! (BitVector Integer Boolean -> Void)]
 [bit-vector-ref  (BitVector Integer -> Boolean)])

(provide small-prime?
         *SMALL-PRIME-LIMIT*)

; The moduli mod 60 that 2, 3 and 5 do not divide are:
;(: non-235 (Listof (Refine [i : Positive-Byte] (<= i 59))))

; Note that there are exactly 16 of these moduli, so they fit in a u16.
; That is, a single u16 can represent a block of 60 numbers.

; <nope> If we were to add a refinement to mod60->bit, we would need to add a refinement
; to (mod60->bit (remainder x 60)) such as (U #f (Refine [b : Byte] (< b (len mod60->bits)))),
; but remainder does not know that being less than 60 is the same as being less than the
; length of mod60->bit.
(: mod60->bit ((Refine [i : Byte] (<= i 59)) -> (U #f Byte)))
(define mod60->bit
  (let ([non-235 : (Listof (Refine [i : Positive-Byte] (<= i 59)))
                 (ann (list 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59)
                      (Listof (Refine [i : Positive-Byte] (<= i 59))))]
        [mod60->bits : (Refine [v : (Vectorof (U #f Byte))] (= 60 (len v))) (make-vector 60 #f)])
    (for ([x : (Refine [i : Positive-Byte] (<= i 59)) (in-list non-235)]
          [b (in-naturals)])
      (safe-vector-set! mod60->bits x (assert b byte?)))
    (λ ([m : (Refine [i : Byte] (<= i 59))])
      (safe-vector-ref mod60->bits m))))

(: *number-of-groups* Positive-Fixnum)
(define *number-of-groups* (ann 17000 Positive-Fixnum)) ; each group contain 16 numbers
(: *SMALL-PRIME-LIMIT* Nonnegative-Fixnum)
(define *SMALL-PRIME-LIMIT* (ann (assert (- (* 60 *number-of-groups*) 1) fixnum?)
                                 Nonnegative-Fixnum))

; primes holds (* 16 *number-of-groups*) bits
; each representing a number not congruent to 2, 3, 5
(define primes
  (let ([non-235 : (Listof (Refine [i : Positive-Byte] (<= i 59)))
                 (ann (list 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59)
                      (Listof (Refine [i : Positive-Byte] (<= i 59))))])
    (make-bit-vector (* (length non-235) *number-of-groups*) #t)))

(define: (clear-bit! [x : Nonnegative-Fixnum]) : Void
  (define q (quotient x 60))
  (define b (mod60->bit (remainder x 60)))
  (when b (bit-vector-set! primes (+ (* q 16) b) #f)))

(define: (inner-bit [q : Nonnegative-Fixnum] [r : (Refine [i : Byte] (<= i 59))]) : Boolean
  (define b (mod60->bit r))
  (if b 
      (bit-vector-ref primes (+ (* q 16) b))
      #f))

(define: (bit [x : Nonnegative-Fixnum]) : Boolean
  (define q (quotient x 60))
  (define r (remainder x 60))
  (inner-bit q r))


(: mark-composites : Nonnegative-Fixnum -> Void)
(define mark-composites
  ; x is prime => mark n*x as prime for all n
  ; Well 2*x, 3*x, 4*x, 5*x, 6*x are not in our table,
  ; so only mark the multiples that are not divisible by 2, 3, or 5.
  (let ([non-235 : (Listof (Refine [i : Positive-Byte] (<= i 59)))
                 (ann (list 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59)
                      (Listof (Refine [i : Positive-Byte] (<= i 59))))])
    (λ ([x : Nonnegative-Fixnum]) : Void
      (let/ec: exit : Void
        (let: loop : Void ([a : Nonnegative-Fixnum 0])
          (define y-base (* a 60 x))
          (for: ([d : Positive-Byte (in-list non-235)])
            (define y (assert (+ y-base (* d x)) fixnum?))
            (when (not (= y x))
              (if (<= y *SMALL-PRIME-LIMIT*)
                  (clear-bit! y)
                  (exit (void)))))
          (loop (assert (add1 a) fixnum?)))))))

(define: sieve-done? : Boolean  #f)

(: sieve (-> Void))
(define sieve
  (let ([non-235 : (Listof (Refine [i : Positive-Byte] (<= i 59)))
                 (ann (list 1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59)
                      (Listof (Refine [i : Positive-Byte] (<= i 59))))])
    (λ () : Void
      (clear-bit! 1) ; 1 is not prime
      (let/ec: exit : Void
        (let: loop : Void ([q : Nonnegative-Fixnum 0])
          (for: ([r : (Refine [i : Positive-Byte] (<= i 59)) (in-list non-235)])
            (define x (assert (+ (* q 60) r) fixnum?))
            (when (> (* x x) *SMALL-PRIME-LIMIT*)
              (exit (void)))
            (when (inner-bit q r) ; x is prime
              (mark-composites x)))
          (loop (assert (add1 q) fixnum?)))))))

(define: (small-prime? [x : Nonnegative-Fixnum]) : Boolean
  (unless sieve-done?
    (sieve)
    (set! sieve-done? #t))
  (or (= x 2) (= x 3) (= x 5) (bit x)))
