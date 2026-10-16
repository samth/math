#lang racket/base
(require rackunit
         (prefix-in core: racket/flonum)
         math/flonum)
(check-eq? flfma core:flfma)
(check-equal? (flfma 1.0000000000000002 0.9999999999999998 -1.0) -4.930380657631324e-32)
(check-equal? (flfma 1e308 2.0 -1e308) 1e308)
(check-equal? (flfma 5e-324 0.5 5e-324) 1e-323)
(check-eqv? (flfma -0.0 1.0 -0.0) -0.0)
(check-equal? (flfma +inf.0 0.0 1.0) +nan.0)
