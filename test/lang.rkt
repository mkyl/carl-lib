#lang racket/base

(require rackunit
    db
    "../lang/main.rkt")

(define lang-tests
    (test-suite
     "CaRL Language Tests"
     (test-case
        "Built AST for simple model"
        (let* ([lst (list 2 4 6 8)]
              [f (open-input-file "test/simple.carl")]
              [m (create-model f)]
              [sqlite (sqlite3-connect #:database 'memory)]
              [_ (populate-db sqlite)]
              [edges (load-data m sqlite)])
            (check = (length edges) 6)))))

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
