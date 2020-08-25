#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 math/matrix
         2htdp/image
		 "estimate/estimate.rkt")

(provide (proc-doc/names estimate (any/c any/c . -> . list?) (unit-table fast) 
	("Estimate the average treatment effect (ATE) given a unit table.")))

