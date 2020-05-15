#lang racket/base

(require racket/list
        racket/sequence
        carl-lib/lang
         graph)

(define (detect model)
    (let* ([g (rules-to-dag model)]
           [c (candidate-stream g)]
           [bc (sequence-filter (λ (x) (backdoor-criterion g x)) c)]
           [Z (sequence-argmin length bc)])           
           (displayln Z)
	    Z))

; convert the set of rules to a DAG, so that graphical
; criterions can be applied to it
(define (rules-to-dag model)
    (let* ([rule-to-edge (λ (r) (list (rule-head r) (rule-body r)))]
           [edges (map rule-to-edge model)])
        (unweighted-graph/directed edges)))

; stream of the powerset of the nodes of g
(define (candidate-stream g)
    (in-combinations (get-vertices g)))

(define (sequence-argmin proc s)
    (cdr (sequence-fold 
        (λ (i x)
           (if (< (proc x) (car i))
               (cons (proc x) x)
                i))
    (cons +inf.0 #f) s)))

; returns whether adjusting for the set Z satisfies
; the backdoor criterion on the graph G. This is sufficient
; but not neccesary for adjustment.
(define (backdoor-criterion G Z)
    #f)

(provide detect)
