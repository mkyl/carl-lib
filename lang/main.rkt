#lang racket/base

(require "reader.rkt"
         "expander.rkt"
         "grounding.rkt")

(define (create-model port)
    (let* ([ast (load-rules port)]
           [model (handle-model ast)])
        model))
(provide create-model)

(provide load-data)
