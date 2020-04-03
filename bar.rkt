#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require csv-reading)

(define BAR_HEIGHT 25)
(define BAR_SPACING 5)
(define MAX_WIDTH 500)
(define INTERP_STEPS 10)
(define INTERP_SMOOTH_EXPT 3)
(define NBARS 15)

(struct dp (name index value))

;; sort datapoints (name . value) by their values
(define (sort-datapoints datapoints)
  (sort datapoints >= #:key cdr))

(define (sort-dps dps)
  (sort dps >= #:key dp-value))

;; computes a semi-unique colour for each string
(define (string->colour s)
  (define strhash (equal-hash-code s))
  (color
   (abs (modulo strhash 256))
   (abs (modulo (arithmetic-shift strhash -16) 256))
   (abs (modulo (arithmetic-shift strhash -32) 256))
   180))

(define (bar width colour)
  (rectangle width BAR_HEIGHT "solid" colour))

(define (bar/label width colour label)
  (beside
   (bar width colour)
   (rectangle 10 0 "solid" "transparent")
   (text label 18 "black")
   ))

;; returns a function that draws a bar on the scale of the current graph
(define (bar-drawer-with-spacing maxval)
  (λ (dp)
    (define spacing
      (* (+ BAR_HEIGHT BAR_SPACING) (dp-index dp)))
    (above
     (rectangle 0 spacing "solid" "transparent")
     (bar/label (* (/ (dp-value dp) maxval) MAX_WIDTH)
                (string->colour (dp-name dp))
                (dp-name dp)))))

(define (x-axis-point val)
  (above/align "left"
               (text (~v val) 16 "black")
               (rectangle 30 1 "solid" "gray")
               (rectangle 1 6 "solid" "gray")
               (rectangle 1 3 "solid" "transparent")))

;; draws an x axis
(define (x-axis maxval)
  (define scale
    (expt 10 (exact-floor (log maxval 10))))
  (define npoints
    (+ 1 (quotient (exact-floor maxval) scale)))
  (foldr overlay/xy
         (rectangle 0 0 "solid" "transparent")
         (map x-axis-point (range 0 (* npoints scale) scale))
         (make-list npoints (* (/ scale maxval) MAX_WIDTH))
         (make-list npoints 0)))

(define (bargraph-interp data)
  (define maxval (apply max (map dp-value data)))
  (above/align "left"
         (x-axis maxval)
         (foldr (λ (image1 image2) (overlay/align "left" "top" image1 image2))
                (rectangle 0 0 "solid" "transparent")
                (map (bar-drawer-with-spacing maxval)
                     (filter
                           (λ (datapoint) (< (dp-index datapoint) NBARS))
                           (sort-dps data))))))


(define next-row
  (make-csv-reader (open-input-file "testing.csv")))

(define users
  (cdr (next-row)))

(define data
  (csv->list next-row))

(define (row->dict row)
  (map cons users row))

(define (row->idxdict indexes row)
  (map list
       users indexes row))

(define (get-indexes row)
  (define sorted-names
    (map car (sort-datapoints (row->dict row))))
  (map
   (λ (name) (index-of sorted-names name))
   users))

(define (interp-fn progress)
  (if (<= progress 0.5)
      (* (expt 2 (- INTERP_SMOOTH_EXPT 1))
         (expt progress INTERP_SMOOTH_EXPT))
      
      (+ 1 (* (expt -2 (- INTERP_SMOOTH_EXPT 1))
              (expt (- progress 1) INTERP_SMOOTH_EXPT)))))

(define (interp a b progress)
  (+ (* a (- 1 (interp-fn progress)))
     (* b (interp-fn progress))))

(define (dp-row row-index interp-step)
  (define row-raw (list-ref data row-index))
  (define row (map string->number (cdr row-raw)))
  (define next-row (map string->number (cdr (list-ref data (min (+ row-index 1) (- (length data) 1))))))
  (define row-interp
    (map (λ (n1 n2)
           (interp n1 n2 (/ interp-step INTERP_STEPS)))
         row
         next-row))
  (define idx-interp
    (map (λ (i1 i2)
           (define better-i1
             (if (and (> i1 NBARS) (<= i2 NBARS))
                 NBARS
                 i1))
           (interp better-i1 i2 (/ interp-step INTERP_STEPS)))
         (get-indexes row)
         (get-indexes next-row)))
  (map dp users idx-interp row-interp))

(define interp-data
  (append-map (λ (row-index)
         (map (λ (interp-step) (dp-row row-index interp-step))
              (range INTERP_STEPS)))
       (range (length data))))

(define (next-frame t)
  (define row (list-ref interp-data t))
  (define date (car (list-ref data (quotient t INTERP_STEPS))))
  (above/align "left"
   (text date 32 "black")
   (bargraph-interp row)))

(animate next-frame)