#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 math/matrix
         2htdp/image
		 "estimate/estimate.rkt")

(provide (proc-doc/names estimate (any/c . -> . image?) (unit-table) 
	("Estimate the average treatment effect (ATE) given a unit table.")))

