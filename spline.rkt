#lang racket

(require math/array)
(require math/matrix)

(define (cubic-spline xvals yvals)
  (define fn (map list xvals yvals))
  (define npoints (length xvals))
  (define npolys (- npoints 1))
  ; matrix of a1 b1 c1 d1 a2 b2 ... an bn cn dn
  (define matrix-size (* npolys 4))
  
  ; S(x) = f(x)
  (define a-part1
    (for/list ([i (in-sequences (in-range npolys) (in-range npolys))]
               [x (in-sequences (drop-right xvals 1) (drop xvals 1))])
      (define row (make-vector matrix-size))
      (define offset (* i 4))
      (vector-set! row offset (expt x 3))
      (vector-set! row (+ offset 1) (expt x 2))
      (vector-set! row (+ offset 2) x)
      (vector-set! row (+ offset 3) 1)
      row))
  (define b-part1 (append (drop-right yvals 1) (drop yvals 1)))

  ; S1'(x2) = S2'(x2) etc
  (define a-part2
    (for/list ([i1 (in-range (- npolys 1))]
               [i2 (in-range 1 npolys)]
               [x (drop-right (drop xvals 1) 1)])
      (define row (make-vector matrix-size))
      (define offset1 (* i1 4))
      (define offset2 (* i2 4))
      (vector-set! row offset1 (* 3 (expt x 2)))
      (vector-set! row (+ offset1 1) (* 2 x))
      (vector-set! row (+ offset1 2) 1)
      (vector-set! row offset2 (* -3 (expt x 2)))
      (vector-set! row (+ offset2 1) (* -2 x))
      (vector-set! row (+ offset2 2) -1)
      row))
  (define b-part2 (make-list (- npoints 2) 0))

  ; S1''(x2) = S2''(x2) etc
  (define a-part3
    (for/list ([i1 (in-range (- npolys 1))]
               [i2 (in-range 1 npolys)]
               [x (drop-right (drop xvals 1) 1)])
      (define row (make-vector matrix-size))
      (define offset1 (* i1 4))
      (define offset2 (* i2 4))
      (vector-set! row offset1 (* 6 x))
      (vector-set! row (+ offset1 1) 2)
      (vector-set! row offset2 (* -6 x))
      (vector-set! row (+ offset2 1) -2)
      row))
  (define b-part3 (make-list (- npoints 2) 0))

  ; S1' = 0, Sn' = 0
  (define a-part4
    (for/list ([i (list 0 (- npolys 1))]
               [x (list (first xvals) (last xvals))])
      (define row (make-vector matrix-size))
      (define offset (* i 4))
      (vector-set! row offset (* 3 (expt x 2)))
      (vector-set! row (+ offset 1) (* 2 x))
      (vector-set! row (+ offset 2) 1)
      row))
  (define b-part4 (make-list 2 0))

  (define a-list (append a-part1
                         a-part2
                         a-part3
                         a-part4))
  (define b-list (append b-part1
                         b-part2
                         b-part3
                         b-part4))


  ; construct matrices
  (define a (build-matrix matrix-size matrix-size
                          (λ (m n) (vector-ref (list-ref a-list m) n))))
  (define b (build-matrix matrix-size 1
                          (λ (m n) (list-ref b-list m))))

  (define coefficients (array->vector (matrix-solve a b)))

  (λ (x)
    (define xpt-index (index-of xvals
                                (last (filter (λ (val) (<= val x))
                                              (drop-right xvals 1)))))
    
    (define offset (* xpt-index 4))
    (+ (* (vector-ref coefficients offset) (expt x 3))
       (* (vector-ref coefficients (+ offset 1)) (expt x 2))
       (* (vector-ref coefficients (+ offset 2)) x)
       (vector-ref coefficients (+ offset 3)))))