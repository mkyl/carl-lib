#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 db
		 carl-lib/lang
		 "ground/load.rkt")

; takes a path, a lifted causal graph, and table info dict 
; returns (tables, WHERE clause)
(define (path-conditions p G TI)
    null)

(struct Z-query-parts (tables where-expr))

(define (build-covar-query parts)
    (define T (Z-query-parts-tables parts))
    (define W (Z-query-parts-where-expr parts))
    null)

(define (ground-direct db t y Z G TI)
    (define P
        (for/list ([z Z])
            (fewest-vertices-path G t z)))
    ; collect parts to build queries
    (define ZQ-parts
        (for/list ([p P])
            (path-conditions p G TI)))
    ; queries for each covariate
    (define ZQ
        (for/list ([qp ZQ-parts])
            (build-covar-query qp)))
    ; query that creates the unit table
    (define unit-table-q
        null)
    ; result
    (query-rows db unit-table-q))

(provide (proc-doc/names ground ((listof rule?) connection?
	. -> . (and/c graph? unweighted-graph?)) (model conn) 
	("Combine CaRL rules with a database instance to build a ground causal model.")))

(provide (contract-out
    [struct atom ((unit list?) (attr symbol?) (value any/c))]))

(provide get-missing
    ground-direct)
