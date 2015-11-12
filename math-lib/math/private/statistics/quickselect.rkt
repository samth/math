#lang typed/racket/base

#|
Quickselect to find the kth order statistic, using random pivot to reduce probability of worst case

Technically O(n^2), but astronomically unlikely; expected running time is O(n)

Seems to be faster than sort-and-direct-ref on sequences with length > 350 or so, not much slower
otherwise
|#

(require typed/safe/ops
         racket/fixnum
         "../unsafe.rkt"
         "statistics-utils.rkt")

(provide kth-value! kth-value)

(: partition! (All (A) (~> ([vs : (Refine [v : (Vectorof A)]
                                          (< start (len v)))]
                            [start : (Refine [start : Fixnum]
                                             (<= 0 start) (< start end))]
                            [end : (Refine [end : Fixnum]
                                           (< start end)
                                           (< end (len vs)))]
                            [lt? : (A A -> Any)])
                           (Refine [res : Fixnum] (<= start res) (<= res end)))))
(define (partition! vs start end lt?)
  ; <nope> The types of start and end are not consistent.
  (let ([p : (Refine [d : Natural] (= d (+ (* -1 start) end)))
           (assert (- end start) exact-nonnegative-integer?)])
    (let ([p : (Refine [a : Natural] (<= 0 a) (< a p))
             (random p)])
      (let ([p : (Refine [p : Integer] (<= 0 p) (< p (len vs)))
               (+ start (+ 1 p))])
        (let ([pivot : A (safe-vector-ref vs p)])
          (safe-vector-set! vs p (safe-vector-ref vs end))
          (safe-vector-set! vs end pivot)
          (let loop ([l : (Refine [l : Fixnum]
                                  (<= start l) (<= l end))
                        start]
                     [r : (Refine [r : Fixnum]
                                  (<= start r) (<= r end))
                        end])
            (cond [(l  . fx< . r)
                   (define v1 (safe-vector-ref vs l))
                   (cond [(lt? v1 pivot)  (loop (unsafe-fx+ l 1) r)]
                         [else
                          (safe-vector-set! vs r v1)
                          (let ([r* : (Refine [r* : Fixnum]
                                              (<= start r*) (<= r* end))
                                    (fx- r 1)])
                            (safe-vector-set! vs l (safe-vector-ref vs r*))
                            (loop l r*))])]
                  [else
                   (safe-vector-set! vs l pivot)
                   l])))))))

(: kth-value!* (All (A) (~> ([v : (Refine [v : (Vectorof A)] (< 0 (len v)))]
                             [k : (Refine [k : Nonnegative-Fixnum] (<= 0 k) (< k (len v)))]
                             [lt? : (A A -> Any)])
                            A)))
(define (kth-value!* vs k lt?)
  (define n (vector-length vs))
  (let loop ([start : (Refine [start : Fixnum] (<= 0 start)) 0]
             [end : (Refine [end : Fixnum] (< end (len vs))) (fx- n 1)]
             [k k])
    (cond [(start . fx< . end)
           (let ([c (partition! vs start end lt?)])
             (define start+k (unsafe-fx+ start k))
             (cond [(c . fx> . start+k)
                    (loop start (unsafe-fx- c 1) k)]
                   [(c . fx< . start+k)
                    (define c+1 (unsafe-fx+ c 1))
                    (loop c+1 end (unsafe-fx- start+k c+1))]
                   [else
                    (unsafe-vector-ref vs start+k)]))]
          [else
           (unsafe-vector-ref vs start)])))

(: kth-value! (All (A) ((Vectorof A) Integer (A A -> Any) -> A)))
(define (kth-value! vs k lt?)
  (define n (vector-length vs))
  (cond [(n . fx<= . 0)  (raise-argument-error 'kth-value! "nonempty Vector" 0 vs k lt?)]
        [(k . < . 0)  (raise-argument-error 'kth-value! "Natural" 1 vs k lt?)]
        [(k . >= . n)  (raise-argument-error 'kth-value! (format "Natural < ~a" n) 1 vs k lt?)]
        [else  (kth-value!* vs k lt?)]))

(: kth-value (All (A) ((Sequenceof A) Integer (A A -> Any) -> A)))
(define (kth-value seq k lt?)
  (define vs (sequence->vector seq))
  (define n (vector-length vs))
  (cond [(n . fx<= . 0)  (raise-argument-error 'kth-value "nonempty Sequence" 0 seq k lt?)]
        [(k . < . 0)  (raise-argument-error 'kth-value "Natural" 1 seq k lt?)]
        [(k . >= . n)  (raise-argument-error 'kth-value (format "Natural < ~a" n) 1 seq k lt?)]
        [else  (kth-value!* vs k lt?)]))
