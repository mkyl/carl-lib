#lang racket/base

(require racket/contract
	     "lang/reader.rkt"
         "lang/expander.rkt")

(provide (contract-out
	 	  [create-model (input-port? . -> . (listof rule?))])
		 (struct-out rule))

(define (create-model port)
    (let* ([ast (load-rules port)]
           [model (handle-model ast)])
        model))
