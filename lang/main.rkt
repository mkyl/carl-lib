#lang racket/base

(require racket/contract
	     "reader.rkt"
         "expander.rkt"
         "grounding.rkt")

(provide (contract-out
	 	  [create-model (input-port? . -> . (listof rule?))]))

(provide load-data)

(define (create-model port)
    (let* ([ast (load-rules port)]
           [model (handle-model ast)])
        model))

