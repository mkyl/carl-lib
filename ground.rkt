#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 db
		 carl-lib/lang
		 "ground/load.rkt")

(provide (proc-doc/names ground ((listof rule?) connection?
	. -> . (and/c graph? unweighted-graph?)) (model conn) 
	("Combine CaRL rules with a database instance to build a ground causal model.")))
