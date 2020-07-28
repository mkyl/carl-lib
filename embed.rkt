#lang racket/base

(require racket/contract
		 math/matrix
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 carl-lib/unit-table
		 "embed/summary.rkt")

(provide (proc-doc/names embed ((listof pre-row?)
	. -> . matrix?) (gcm) 
	("Apply embeddings to a semi-structured table to create flat table.")))

(define embed 
	summarize)
