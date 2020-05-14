#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 racket/list
		 carl-lib/lang
		 "detect/backdoor.rkt")

(provide (proc-doc/names detect ((listof rule?)
                                 . -> . list?) (model) 
	("Return a minimal set of sufficient covariates for adjustment.")))
