#lang racket
(require racket/dict
         math/distributions)

;; From https://rosettacode.org/wiki/K-means%2B%2B_clustering#Racket
 
;; Divides the set of points into k clusters
;; using the standard k-means clustering algorithm
(define (k-means data k #:initialization (init k-means++))
  (define (iteration centroids)     
    (map centroid (clusterize data centroids)))
  (fixed-point iteration (init data k) #:same-test small-shift?))
 
;; Finds the centroid for a set of points
(define (centroid pts)
  (vector-map (curryr / (length pts))
       (for/fold ([sum (car pts)]) ([x (in-list (cdr pts))])
         (vector-map + x sum))))
 
;; Divides the set of points into clusters
;; using given centroids
(define (clusterize data centroids)
  (for*/fold ([res (map list centroids)]) ([x (in-list data)])
    (define c (argmin (distanse-to x) centroids))
    (dict-set res c (cons x (dict-ref res c)))))
 
;; Stop criterion: all centroids change their positions
;; by less then 0.1% of the minimal distance between centroids.
(define (small-shift? c1 c2)
  (define min-distance
    (apply min
           (for*/list ([x (in-list c2)]
                       [y (in-list c2)] #:unless (equal? x y))
             ((metric) x y))))
  (for/and ([a (in-list c1)] [b (in-list c2)])
    (< ((metric) a b) (* 0.001 min-distance))))

;; picks k points from a dataset randomly
(define (random-choice data k)
  (for/list ([i (in-range k)])
    (list-ref data (random (length data)))))
 
;; uses k-means++ algorithm
(define (k-means++ data k)
  (for/fold ([centroids (random-choice data 1)]) ([i (in-range (- k 1))])
    (define weights
      (for/list ([x (in-list data)])
        (apply min (map (distanse-to x) centroids))))
    (define new-centroid
      (sample (discrete-dist data weights)))
    (cons new-centroid centroids)))

 
(define (euclidean-distance a b)
  (for/sum ([x (in-vector a)] [y (in-vector b)]) 
    (sqr (- x y))))
  
(define metric (make-parameter euclidean-distance))
(define (distanse-to x) (curry (metric) x))

(define (fixed-point f x0 #:same-test [same? equal?])
  (let loop ([x x0] [fx (f x0)])
    (if (same? x fx) fx (loop fx (f fx)))))

(provide k-means
    euclidean-distance)