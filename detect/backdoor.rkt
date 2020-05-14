#lang racket/base

(require racket/list
        carl-lib/lang
         graph)

(define (detect model)
    (let* ([g (rules-to-dag model)]
           ; warning: candidate-stream is exponential space in the nodes of G.
           [c (candidate-stream g)]
           [c (filter backdoor-criterion c)]
           [Z (argmin length c)])
	    Z))

; convert the set of rules to a DAG, so that graphical
; criterions can be applied to it
(define (rules-to-dag model)
    (let* ([rule-to-edge (λ (r) (cons (rule-head r) (rule-body r)))]
           [edges (map rule-to-edge model)])
        (unweighted-graph/directed edges)))

; stream of the powerset of the nodes of g
(define (candidate-stream g)
    (in-combinations (get-vertices g)))

; returns whether adjusting for the set Z satisfies
; the backdoor criterion on the graph G. This is sufficient
; but not neccesary for adjustment.
(define (backdoor-criterion G Z)
    #f)

(provide detect)
