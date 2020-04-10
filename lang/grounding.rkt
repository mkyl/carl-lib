#lang racket

(require db)
(require "expander.rkt")

(define (load-data model dbc)
    (let* ([edges (map (λ (m) (fetch-edges m dbc)) model)]
          [edges (foldl append '() edges)]) 
        edges))

(define (fetch-edges rule dbc)
    (let* ([h (query-rows dbc (string-append "select key, \"" (first (rest (rule-head rule))) "\", value from " (first (rest (rule-head rule))) " order by key"))]
           [b (query-rows dbc (string-append "select key, \"" (first (rest (rule-body rule))) "\", value from " (first (rest (rule-body rule))) " order by key"))]
           [result (map list b h)])
        result))
        
(provide load-data)
