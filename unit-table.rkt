#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 carl-lib/lang
		 graph
		 math/matrix)

(provide (proc-doc/names construct 
	((and/c graph? unweighted-graph?) causal-q? list? . -> . matrix?)
	(augmented-gcm query Z) 
	("Construct a unit table given an augmented GCM and a set of covariates.")))

(define (construct augmented-gcm q Z)
	(matrix [[0]]))
