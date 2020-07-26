#lang racket/base

(require 
	carl-lib/lang
	carl-lib/ground
	math/matrix
	math/array
	racket/list
	racket/set
	graph)

(define (construct aug-gcm q Z)
	(let* ([Y (predicate-name (c-query-outcome q))]
		   [T (predicate-name (c-query-treatment q))]
		   [g (unweighted-graph/undirected (get-edges aug-gcm))]
		   [vs (get-vertices g)]
		   [outcomes (filter (lambda (x) (equal? (atom-attr x) Y)) vs)]
		   [path-len (map (λ (v)
		   					(let-values ([(h _) (dijkstra g v)]) h)) outcomes)]
		   [treatments (map (lambda (x pl)
		    					(find-nodes g pl (set T) x)) outcomes path-len)]
		   [covariates (map (lambda (x pl)
		   						(find-nodes g pl (set-map Z predicate-name) x))
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
				(set-member? N (atom-attr x))))
		(get-vertices g)))

(define (produce-row y ts zs)
	(append (list (atom-value y))
			(map (λ (t) (atom-value t)) ts)
		 	(map (λ (z) (atom-value z)) zs)))

(provide construct)
