#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 racket/list
		 carl-lib/lang
		 "detect/backdoor.rkt")

(provide (proc-doc/names detect ((listof rule?)
                                 . -> . (or/c list? #f)) (model) 
	("Return a minimal set of sufficient covariates for adjustment. #f if no such set found.")))
