#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
	     "lang/reader.rkt"
         "lang/expander.rkt")

(provide (proc-doc/names
		  create-model (input-port? . -> . (listof (or/c rule? ques?))) (port) 
		  ("Read a file in the CaRL language and output the set of 
		  	rules and queries it contains."))
		 (struct-out rule)
		 (struct-out ques))

(define (create-model port)
    (let* ([ast (load-rules port)]
           [model (handle-model ast)])
        model))
