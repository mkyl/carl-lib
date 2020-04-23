#lang racket/base

(require 
	carl-lib/lang
	math/matrix
	math/array
	racket/list
	graph)

(define (construct aug-gcm q Z)
	(let* ([Y (table-name (causal-q-outcome q))]
		   [T (table-name (causal-q-treatment q))]
		   [vs (get-vertices aug-gcm)]
		   [outcomes (filter (lambda (x) (equal? (vector-ref x 1) Y)) vs)]
		   ; flip the edges of G so that treatments are neighbors of outcomes
		   [g_rev (transpose aug-gcm)]
		   [treatments (map (lambda (x) (find-treatment g_rev T x)) outcomes)]
		   [data (map produce-row outcomes treatments)]
		   [result (list->matrix (length outcomes) 2 (flatten data))])
		result))

; given a treatment name T and a outcome node Y-node
; return the corresponding treatment node
(define (find-treatment aug-gcm T Y-node)
	(findf (lambda (x) (equal? (vector-ref x 1) T)) (get-neighbors aug-gcm Y-node)))

(define (produce-row y t)
	(list (vector-ref y 2) (vector-ref t 2)))

(provide construct)
