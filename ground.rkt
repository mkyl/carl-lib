#lang racket/base

(require racket/contract
		 graph
		 db
		 carl-lib/lang
		 "ground/load.rkt")

(provide (contract-out [ground ((listof rule?) connection?
	. -> . (and/c graph? unweighted-graph?))]))
