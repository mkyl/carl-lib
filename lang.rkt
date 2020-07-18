#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
	     "lang/reader.rkt"
         "lang/expander.rkt")

(provide (proc-doc/names
		  create-inputs (input-port? . -> . inputs?) (port) 
		  ("Read a file in the CaRL language and output the set of 
		  	rules and queries it contains."))
		 (struct-out rule)
         (struct-out c-query)
         (struct-out inputs)
         (struct-out predicate))

(define (create-inputs port)
    (let* ([ast (load-rules port)]
           [inputs (handle-inputs ast)])
        inputs))
