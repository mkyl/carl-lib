#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 "embed/summary.rkt")

(provide (proc-doc/names embed ((and/c graph? unweighted-graph?)
	. -> . (and/c graph? unweighted-graph?)) (gcm) 
	("Apply embeddings to a ground causal model (GCM) to create the augmented GCM.")))

(define embed 
	summarize)
