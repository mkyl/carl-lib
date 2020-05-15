#lang racket/base

(require racket/list
        racket/sequence
        carl-lib/lang
         graph)

(define (detect model t y)
    (let* ([g (rules-to-dag model)]
           [c (candidate-stream g)]
           [bc (sequence-filter (λ (x) (backdoor-criterion g t y x)) c)]
           [Z (sequence-argmin length bc)])           
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

; Returns whether adjusting for the set Z satisfies
; the backdoor criterion on the graph G, given treatment
; T and outcome Y. This is sufficient but not neccesary for adjustment.
(define (backdoor-criterion G T Y Z)
    (let* ([e (get-edges G)]
           ; remove edges that leave T (leaving only "backdoor" paths)
           [e_ (filter (λ (x) (not (equal? (car x) T))) e)]
           [e_^2 (cartesian-product e_ e_)]
           [op (filter (λ (x) (path-open (car x) (second x) Z)) e_^2)]
           [ps (map (λ (x) (link-edges (car x) (second x))) op)]
           [g_b (unweighted-graph/undirected ps)]
           ; ensure that T and Y are always in the graph
           [_ (add-vertex! g_b T)]
           [_ (add-vertex! g_b Y)]
           ; try to find a path between T and Y
           [r (fewest-vertices-path g_b T Y)]
           ; if no such path, backdoor criterion is satisfied
           [conn (equal? r #f)])
        conn))

; Determines whether a 3-vertex causal path is open. 
; (as opposed to blocked or d-seperated)
(define (path-open e1 e2 Z)
    (let ([a (car e1)]
          [b (second e1)]
          [c (car e2)]
          [d (second e2)])
        (cond 
            ; fork
            [(and (equal? a c) (not (equal? b d))) (not (in? a Z))]
            ; chain
            [(and (equal? b c) (not (equal? a d))) (not (in? b Z))]
            ; collider
            [(and (equal? b d) (not (equal? a c))) (in? b Z)]
            ; all others
            [else #f])))

(define (in? m list)
    (not (equal? (member m list) #f)))

(define (link-edges e1 e2)
    (list (car e1) (second e2)))

(provide detect
    backdoor-criterion)
