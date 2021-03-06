#lang typed/racket

(require/typed racket/list
               [index-of (-> (Listof Any) Any Natural)])
(require math/array)
(require math/matrix)

(: get-spline-fns (-> (Listof Real) (Listof Real) (Vectorof Real)))
(define (get-spline-fns xvals yvals)
  (define npoints (length xvals))
  (define npolys (- npoints 1))
  ; matrix of a1 b1 c1 d1 a2 b2 ... an bn cn dn
  (define matrix-size (* npolys 4))
  
  ; S(x) = f(x)
  (: a-part1 (Listof (Vectorof Real)))
  (define a-part1
    (for/list ([i (in-sequences (in-range npolys) (in-range npolys))]
               [x (in-sequences (drop-right xvals 1) (drop xvals 1))])
      (: row (Vectorof Real))
      (define row (make-vector matrix-size))
      (define offset (* i 4))
      (vector-set! row offset (expt x 3))
      (vector-set! row (+ offset 1) (expt x 2))
      (vector-set! row (+ offset 2) x)
      (vector-set! row (+ offset 3) 1)
      row))
  (define b-part1 (append (drop-right yvals 1) (drop yvals 1)))

  ; S1'(x2) = S2'(x2) etc
  (: a-part2 (Listof (Vectorof Real)))
  (define a-part2
    (for/list ([i1 (in-range (- npolys 1))]
               [i2 (in-range 1 npolys)]
               [x (drop-right (drop xvals 1) 1)])
      (: row (Vectorof Real))
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
  (: a-part3 (Listof (Vectorof Real)))
  (define a-part3
    (for/list ([i1 (in-range (- npolys 1))]
               [i2 (in-range 1 npolys)]
               [x (drop-right (drop xvals 1) 1)])
      (: row (Vectorof Real))
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
  (: a-part4 (Listof (Vectorof Real)))
  (define a-part4
    (for/list ([i (list 0 (- npolys 1))]
               [x (list (first xvals) (last xvals))])
      (: row (Vectorof Real))
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
                          (λ ([m : Integer] [n : Integer]) (vector-ref (list-ref a-list m) n))))
  (define b (build-matrix matrix-size 1
                          (λ ([m : Integer] [n : Integer]) (list-ref b-list m))))

  (array->vector (matrix-solve a b))
)

(: straight-line (-> Real (Vectorof Real)))
(define (straight-line y)
  ;; a straight line represented by ax^3 + bx^2 + cx + d
  (vector 0 0 0 y))            

(: vector-flatten (All (a) (-> (Listof (Vectorof a)) (Vectorof a))))
(define (vector-flatten lv)
  (apply vector-append lv))

(: get-fns (-> (Listof Real) (Listof Real) (Vectorof Real)))
(define (get-fns xvals yvals)
  (define len (length yvals))
  
  (: fns (Listof (Vectorof Real)))
  (: _ Natural)
  (define-values (fns _)
    (for/fold ([fns : (Listof (Vectorof Real)) '()]
               [start-index : Natural 0])
              ([_ (in-range +inf.0)]
               #:break (= start-index (sub1 len)))
      (define lst-tail (drop yvals start-index))
      (define val (first lst-tail))
      (define straight-line? (= val (second lst-tail)))
      (: end-index Natural)
      (define end-index
        (for/fold ([end-index : Natural 0])
                  ([end-val (in-list (rest lst-tail))]
                   #:break (if straight-line?
                               (not (= val end-val))
                               (= val end-val)))
          (set! val end-val)
          (add1 end-index)))
      
      (values
       (if straight-line?
           (append fns (make-list end-index (straight-line val)))
           (append fns (list (get-spline-fns
                              (take (drop xvals start-index) (add1 end-index))
                              (take lst-tail (add1 end-index))))))
       (+ start-index end-index))))
  (vector-flatten fns))

(: cubic-spline (-> (Listof Real) (Listof Real) (-> Real Real)))
(define (cubic-spline xvals yvals)
  (define coefficients (get-fns xvals yvals))
  (displayln "did spline")
  
  (λ (x)
    (define xpt-index (index-of xvals
                                (last (filter (λ ([val : Real]) (<= val x))
                                              (drop-right xvals 1)))))
    (define offset (* xpt-index 4))
    
    ; this check is to make sure we don't keep using the splines
    ; once we're out of the domain of the entire thing (the function
    ; should continue to return the last y value
    (if (>= x (last xvals))
        (last yvals)
        (+ (* (vector-ref coefficients offset) (expt x 3))
           (* (vector-ref coefficients (+ offset 1)) (expt x 2))
           (* (vector-ref coefficients (+ offset 2)) x)
           (vector-ref coefficients (+ offset 3))))))

(provide cubic-spline)