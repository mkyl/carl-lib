#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 math/matrix)

(provide (proc-doc/names foo (matrix? . -> . real?) (unit-table) 
	("Estimate the average treatment effect (ATE) given a unit table.")))

(define foo
	(raise 'failed #t))
