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
              [m (create-inputs f)]
              [sqlite (sqlite3-connect #:database 'memory)]
              [_ (populate-db sqlite)]
              [g (ground (inputs-rules m) sqlite)]
              [g_undir (undirected-graph (get-edges g))])
            ; expect 6 edges, 9 vertices
            (check = (length (get-edges g)) 6)
            (check = (length (get-vertices g)) 9)
            ; graph should have 3 connected components (ignoring direction)
            (check = (length (cc g_undir)) 3)))
     (test-case
        "Handles where clauses reasonably"
        (let* ([f (open-input-bytes 
                    #"Y[X] <- T[Y] where A[X], B[Y] \nY[X]<-T[Y] where C[X, Y]?")]
               [m (create-inputs f)]
               [r (car (inputs-rules m))]
               [q (car (inputs-queries m))]
               [r-where (rule-where r)]
               [q-where (c-query-where q)]
               [q-where-vars (predicate-vars (car q-where))])
            (check = (length r-where) 2)
            (check = (length q-where) 1)
            (check = (length q-where-vars) 2)))
     (test-case
        "Correct number of rules and queries"
        (let* ([f (open-input-file "test/simple.carl")]
               [m (create-inputs f)])
            ; one query
            (check = (length (inputs-queries m)) 1)
            ; two rules
            (check = (length (inputs-rules m)) 2)))))

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
