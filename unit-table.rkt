#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 math/matrix)

(provide (proc-doc/names construct 
	((and/c graph? unweighted-graph?) list? . -> . matrix?) (augmented-gcm Z) 
	("Construct a unit table given an augmented GCM and a set of covariates.")))

(define (construct augmented-gcm Z)
	(matrix [[0]]))
