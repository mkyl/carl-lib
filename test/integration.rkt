#lang racket/base

(require rackunit
	racket/lazy-require
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
            (check = ate 0.6)))))

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