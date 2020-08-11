#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 racket/list
		 carl-lib/lang
		 "detect/backdoor.rkt")

(provide (proc-doc/names detect ((listof rule?) (listof predicate?) any/c any/c
                                 . -> . (or/c list? #f)) (model missing T Y) 
	("Return a minimal set of sufficient covariates for adjustment of T on Y.
	 #f if no such set found. Set will not contain any elements from missing.")))
