#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
	     "lang/reader.rkt"
         "lang/expander.rkt")

(provide (proc-doc/names
		  create-model (input-port? . -> . (listof rule?)) (port) 
		  ("Read a file in the CaRL language and output the set of 
		  	rules it contains."))
		 (struct-out rule))

(define (create-model port)
    (let* ([ast (load-rules port)]
           [model (handle-model ast)])
        model))
