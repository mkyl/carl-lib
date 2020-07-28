#lang racket/base

(require 
	carl-lib/lang
	carl-lib/ground
	racket/list
	racket/set
	racket/contract
	graph)

(struct pre-row (y ts zs))

(provide (contract-out [struct pre-row ((y atom?)
          					            (ts (listof atom?))
          				   		 		(zs (listof atom?)))]))

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
		   [data (map produce-row outcomes treatments covariates)])
		data))

; given node type N and a outcome node Y-node
; return all nodes with a possible causal path
(define (find-nodes g path-len N Y-node)
	(filter (λ (x) (and 
				(exact-positive-integer? (hash-ref path-len x))
				(set-member? N (atom-attr x))))
		(get-vertices g)))

(define (produce-row y ts zs)
	(pre-row y ts zs))

(provide construct)
