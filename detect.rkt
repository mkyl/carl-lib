#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 racket/list)

(provide (proc-doc/names detect ((and/c graph? unweighted-graph?)
                                 . -> . list?) (aug-gcm) 
	("Return a set of sufficient covariates for adjustment.")))

(define (detect aug-gcm)
	empty)
