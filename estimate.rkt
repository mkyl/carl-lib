#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 math/matrix
		 "estimate/estimate.rkt")

(provide (proc-doc/names estimate (matrix? . -> . real?) (unit-table) 
	("Estimate the average treatment effect (ATE) given a unit table.")))

