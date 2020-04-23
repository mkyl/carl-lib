#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph)

(provide (proc-doc/names embed ((and/c graph? unweighted-graph?)
	. -> . (and/c graph? unweighted-graph?)) (gcm) 
	("Apply embeddings to a ground causal model (GCM) to create the augmented GCM.")))

; TODO embedding is no-op at the moment
(define (embed gcm) 
	gcm)
