#lang racket/base

(require 
	carl-lib/lang
	math/matrix
	math/array
	racket/list
	racket/set
	graph)

(define (construct aug-gcm q Z)
	(let* ([Y (table-name (causal-q-outcome q))]
		   [T (table-name (causal-q-treatment q))]
		   [g (unweighted-graph/undirected (get-edges aug-gcm))]
		   [vs (get-vertices g)]
		   [outcomes (filter (lambda (x) (equal? (vector-ref x 1) Y)) vs)]
		   [path-len (map (λ (v)
		   					(let-values ([(h _) (dijkstra g v)]) h)) outcomes)]
		   [treatments (map (lambda (x pl)
		    					(find-nodes g pl (set T) x)) outcomes path-len)]
		   [covariates (map (lambda (x pl)
		   						(find-nodes g pl (set-map Z table-name) x))
								    outcomes path-len)]
		   [data (map produce-row outcomes treatments covariates)]
		   [result (list->matrix (length outcomes) (+ 2 (length Z))
		   	 			(flatten data))])
		result))

; given node type N and a outcome node Y-node
; return all nodes with a possible causal path
(define (find-nodes g path-len N Y-node)
	(filter (λ (x) (and 
				(exact-positive-integer? (hash-ref path-len x))
				(set-member? N (vector-ref x 1))))
		(get-vertices g)))

(define (produce-row y ts zs)
	(append (list (vector-ref y 2))
			(map (λ (t) (vector-ref t 2)) ts)
		 	(map (λ (z) (vector-ref z 2)) zs)))

(provide construct)
