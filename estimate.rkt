#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual))

(provide (proc-doc/names foo (number? . -> . number?) (x) 
	("Placeholder")))

(define foo
	(raise 'failed #t))
