#lang errortrace racket/base

(require rackunit
	racket/lazy-require
    racket/stream
    racket/list
	db)

; lazy-require used to avoid cycle between main.rkt <-> integration.rkt
(lazy-require [carl-lib (compute)])

(define integration-tests
    (test-suite
     "CaRL Integration Tests"
     (test-case
        "end-to-end test with simple model and synthetic data"
        (let* ([model (open-input-file "test/simple.carl")]
               [sqlite (sqlite3-connect #:database 'memory)]
               [_ (populate-db sqlite)]
               [ate (compute model sqlite)])
            ; ATE from db below
            (check = ate 0.5)))
     (test-case
        "ATE of simplest confounding model"
        (let* ([target-ate 0]
               [ε 0.05]
               [model (open-input-file "test/confounding.carl")]
               [sqlite (sqlite3-connect #:database 'memory)]
               [_ (populate-confounding sqlite target-ate)]
               [ate (compute model sqlite)])
            (check-= target-ate ate ε)))))

(provide integration-tests)

; TODO deduplicate this code (same in test/lang.rkt)
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

; populate a database instance with 3 tables--T, Y, Q--where Q is a confounder
; of T and Y.
(define (populate-confounding conn ate)
    (let* ([n 1000]
           [units (stream->list (in-range n))]
           [Q (map (lambda (_) (random 2)) units)]
           ; TODO take ate var into account
           [T (map (lambda (q) (if (> (+ 0.25 (* 0.5 q)) (random)) 1 0)) Q)]
           [Y (map (lambda (t q) (if (> (+ (* 0.5 q) (* ate t)) (random)) 1 0)) T Q)])
        (for ([(name content) (in-parallel (list "T" "Y" "Q") (list T Y Q))])
            (query-exec conn (string-append-immutable 
                "create table " name
                " (key string PRIMARY KEY, value integer)"))
            (for ([(k v) (in-parallel units content)])
                (query-exec conn 
                    (string-append-immutable "INSERT into " name
                     " values (?, ?)")
                    k v)))))
    