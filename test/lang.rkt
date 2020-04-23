#lang racket/base

(require rackunit
    db
    graph
    carl-lib/lang
    carl-lib/ground)

(provide lang-tests)

(define lang-tests
    (test-suite
     "CaRL Language Tests"
     (test-case
        "GCM generation for simplest model"
        (let* ([f (open-input-file "test/simplest.carl")]
              [m (create-model f)]
              [sqlite (sqlite3-connect #:database 'memory)]
              [_ (populate-db sqlite)]
              [g (ground (model-rules m) sqlite)]
              [g_undir (undirected-graph (get-edges g))])
            ; expect 6 edges, 9 vertices
            (check = (length (get-edges g)) 6)
            (check = (length (get-vertices g)) 9)
            ; graph should have 3 connected components (ignoring direction)
            (check = (length (cc g_undir)) 3)))
     (test-case
        "Correct number of rules and queries"
        (let* ([f (open-input-file "test/simple.carl")]
               [m (create-model f)])
            ; one query
            (check = (length (model-queries m)) 1)
            ; two rules
            (check = (length (model-rules m)) 2)))))

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
