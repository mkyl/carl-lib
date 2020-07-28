#lang racket/base

(require racket/list
	     math/matrix
		 math/array
		 carl-lib/ground
		 carl-lib/unit-table)

(define THRESHOLD 0.3)

(define (summarize almost-ut)
	(let* ([binary-t (binarize-treatment almost-ut)]
		   [embeded (map embed binary-t)]
		   [cols (+ 2 (length (third (first embeded))))]
		   [result (list->matrix (length embeded) cols
		   	 			(flatten embeded))])
		result))

(define (mean l)
	(/ (apply + l) (length l)))

(define (b2i x)
  (or (and x 1) 0))

(define (binarize-treatment xs)
	(let* ([get-v (lambda (ys) (map atom-value ys))]
		   [treated (lambda (x) (b2i (> x THRESHOLD)))]
		   [f (compose treated mean get-v pre-row-ts)]
		   [vs (map f xs)]
		   [result (map (lambda (x v) (list (pre-row-y x) v (pre-row-zs x)))
		    			xs vs)])
		result))

(define (embed row)
	(let* ([y (atom-value (first row))]
		   [t (second row)]
		   [covars (third row)]
		   [groups (group-by atom-attr covars)]
		   [values (map (lambda (g) (map atom-value g)) groups)]
		   ; TODO ensure sorted
		   [processed (map mean values)])
		(list y t processed)))

(provide summarize)
