#lang racket/base

(require rackunit
    db
    graph
    racket/list
    carl-lib/lang
    carl-lib/ground)

(provide ground-tests)

(define ground-tests
	(test-suite
		"Grounding unit tests"
		(test-case
			"simple join, one-to-one"
			(let* ([d (one-to-one)])
				(check = 1 1)))
		(test-case
			"multiple one-to-one joins"
			(let* ([d (multiple-joins)])
				(check = 1 1)))
		(test-case
			"many-to-one join"
			(let* ([d (many-to-one)])
				(check = 1 1)))))

(define rot13
	;; ROT13 (i.e. Caesar cipher) dict of integers
	(map (lambda (i) (cons i (modulo (+ i 13) 26))) (range 26)))

(define (populate-simple conn)
	(query-exec conn
    "create table letter_from (key integer PRIMARY KEY, value string)")
    (query-exec conn
    "create table mapping (key integer PRIMARY KEY, value integer)")
    (query-exec conn
    "create table letter_to (key integer PRIMARY KEY, value string)"))

(define (populate-multiple conn)
	conn)

(define (populate-many conn)
	conn)

(define (one-to-one)
	;; simplest join, one-to-one
	(let* ([sqlite (sqlite3-connect #:database 'memory)]
		   [_ (populate-simple sqlite)])
		sqlite))

(define (multiple-joins)
	;; two joins
	(let* ([sqlite (sqlite3-connect #:database 'memory)]
		   [populate-multiple sqlite])
	    sqlite))

(define (many-to-one)
	;; a many-to-one join
	(let* ([sqlite (sqlite3-connect #:database 'memory)]
		   [populate-many sqlite])
	    sqlite))
