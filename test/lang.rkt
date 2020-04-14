#lang racket/base

(require rackunit
    db
    graph
    "../lang/main.rkt")

(define lang-tests
    (test-suite
     "CaRL Language Tests"
     (test-case
        "integration test for simple model and db graph gen"
        (let* ([lst (list 2 4 6 8)]
              [f (open-input-file "test/simple.carl")]
              [m (create-model f)]
              [sqlite (sqlite3-connect #:database 'memory)]
              [_ (populate-db sqlite)]
              [edges (load-data m sqlite)]
              [g (directed-graph edges)]
              [g_undir (undirected-graph edges)])
            ; expect 6 edges, 9 vertices
            (check = (length (get-edges g)) 6)
            (check = (length (get-vertices g)) 9)
            ; graph should have 3 connected components (ignoring direction)
            (check = (length (cc g_undir)) 3)))))

(define (populate-db conn)
    (query-exec conn
    "create table score (key string PRIMARY KEY, value integer)")
    (query-exec conn
    "create table experience (key string PRIMARY KEY, value integer)")
    (query-exec conn
    "create table luck (key string PRIMARY KEY, value integer)")

    (query-exec conn
    "insert into score values ('eve', 1), ('alice', 0), ('bob', 1)")
    (query-exec conn
    "insert into experience values ('eve', 0), ('alice', 0), ('bob', 1)")
    (query-exec conn
    "insert into luck values ('eve', 1), ('alice', 0), ('bob', 0)"))

; draw the ground causal model
; (require graph)
; (define g (unweighted-graph/directed edges))
; (display (graphviz g))
