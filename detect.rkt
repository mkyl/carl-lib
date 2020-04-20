#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph)

(provide (proc-doc/names detect ((and/c graph? unweighted-graph?)
                                 . -> . list?) (x) 
	("Return a set of sufficient covariates for adjustment.")))

(define detect
	(raise 'failed #t))
