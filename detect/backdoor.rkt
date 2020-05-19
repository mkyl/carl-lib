#lang errortrace racket/base

(require racket/list
        racket/sequence
        racket/set
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
    (let* ([rule-to-edge (λ (r) (list (rule-body r) (rule-head r)))]
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
    (if (or (in? Y Z) (in? T Z)) #f
    (let* ([e (get-edges G)]
           ; remove edges that leave T (leaving only "backdoor" paths)
           [e_ (filter (λ (x) (not (equal? (car x) T))) e)]
           [e_^2 (map list->set (combinations e_ 2))]
           ; open paths
           [op (filter (λ (x) (path-open (set-first x) (set-first (set-rest x)) Z)) e_^2)]
           ; drop the directions on the edges
           [op_un (map (λ (e) (set-map e list->set)) op)]
           ; build a graph of paths
           [g_b (path-graph op_un T Y)]
           ; try to find a path between T and Y
           [r (fewest-vertices-path g_b T Y)]
           ; if no such path, backdoor criterion is satisfied
           [conn (equal? r #f)])
        conn)))

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
            [(and (equal? a d) (not (equal? b c))) (not (in? a Z))]
            ; collider
            [(and (equal? b d) (not (equal? a c))) (in? b Z)]
            ; all others
            [else #f])))

(define (in? m list)
    (not (equal? (member m list) #f)))

(define (path-graph edge-pairs T Y) 
    (let* ([g (unweighted-graph/undirected empty)]
           [_ (map (λ (x) (add-edge! g (set-first x) (set-first (set-rest x)))) edge-pairs)]
           [v_Y (filter (λ (v) (set-member? (set-map v (λ (x) (equal? x Y))) #t))
                 (get-vertices g))]
           [v_T (filter (λ (v) (set-member? (set-map v (λ (x) (equal? x T))) #t))
                 (get-vertices g))])
           (map (λ (v) (add-edge! g v Y)) v_Y)
           (map (λ (v) (add-edge! g v T)) v_T)
           (add-vertex! g T)
           (add-vertex! g Y)
            g))

(define (link-edges e1 e2)
    (let ([a (car e1)]
          [b (second e1)]
          [c (car e2)]
          [d (second e2)])
          (cond
            [(equal? a c) (list (list b a) (list c d))]
            [(equal? b d) (list (list a b) (list d c))]
            [(equal? b c) (list (list a b) (list c d))]
            [(equal? a d) (list (list b a) (list d c))]
            [else (error "not possible")])))

(provide detect
    backdoor-criterion)
