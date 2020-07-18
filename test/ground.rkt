#lang racket/base

(require rackunit
    db
    graph
    racket/list
    racket/hash
    carl-lib/lang
    carl-lib/ground)

(provide ground-tests)

; how many mock rows to add to testing DB
(define ROWS 25)

(define ground-tests
	(test-suite
		"Grounding unit tests"
		(test-case
			"simple join, one-to-one"
			(let* ([d (one-to-one)]
				   [g (ground model-simple d)])
				(for ([c (map string alphabet)])
					(check-eq? (length (get-neighbors g c)) 1)
					(check-eq? (first (get-neighbors g c)) (hash-ref rot13-char c)))))
		(test-case
			"multiple one-to-one joins"
			(let* ([d (multiple-joins)]
				   [g (ground model-multiple d)])
				(for ([c (map string alphabet)])
					(check-eq? (length (get-neighbors g c)) 1)
					; rot13 twice is identity
					(check-eq? (first (get-neighbors g c)) c))))
		(test-case
			"many-to-one join"
			(let* ([d (many-to-one)]
				   [g (ground model-many d)])
				(for ([i (range ROWS)])
					; the node with value i has i neighbors
					(check-eq? (length (get-neighbors g i)) i))))))

(define rot13
	;; ROT13 (i.e. Caesar cipher) dict of integers
	(map (lambda (i) (cons i (modulo (+ i 13) 26))) (range 26)))

(define rot13-char
	; TODO fixme: this is a map of int:int not char:char
	(apply hash (flatten rot13)))

(define alphabet
	(map integer->char (range (char->integer #\a)
                        (add1 (char->integer #\z)))))

(define (populate-simple conn)
	(query-exec conn
    "create table letter_from (k integer PRIMARY KEY, v string)")
    (query-exec conn
    "create table mapping (k integer PRIMARY KEY, v integer)")
    (query-exec conn
    "create table letter_to (k integer PRIMARY KEY, v string)")
    (for ([c (map string alphabet)])
    	(query-exec conn "insert into letter_from(v) values (?)" c))
    (for ([t rot13])
    	(query-exec conn "insert into mapping(k, v) values (?, ?)" (car t) (cdr t)))
    (for ([c (map string alphabet)])
    	(query-exec conn "insert into letter_to(v) values (?)" c)))

(define (populate-multiple conn)
	(query-exec conn
    "create table letter_from (k integer PRIMARY KEY, v string)")
    (query-exec conn
    "create table mapping1 (k integer PRIMARY KEY, v integer)")
    (query-exec conn
    "create table mapping2 (k integer PRIMARY KEY, v integer)")
    (query-exec conn
    "create table letter_to (k integer PRIMARY KEY, v string)")
    (for ([c (map string alphabet)])
    	(query-exec conn "insert into letter_from(v) values (?)" c))
    (for ([c (map string alphabet)])
    	(query-exec conn "insert into letter_to(v) values (?)" c))
    ; rot13 twice is identity
    (for ([t rot13])
    	(query-exec conn "insert into mapping1(k, v) values (?, ?)" 
    		(car t) (cdr t)))
    (for ([t rot13])
    	(query-exec conn "insert into mapping2(k, v) values (?, ?)"
    		(car t) (cdr t))))

(define (populate-many conn)
	(query-exec conn
    "create table treatment (k integer PRIMARY KEY, v string)")
    (query-exec conn
    "create table mapping (k integer PRIMARY KEY, v integer)")
    (query-exec conn
    "create table outcome (k integer PRIMARY KEY, v string)")
    (for ([i (range ROWS)])
    	(query-exec conn "insert into treatment(v) values (?)" i))
    (for ([i (range ROWS)])
    	(query-exec conn "insert into outcome(v) values (?)" i))

    ; insert 1 one time, 2 two times, ..., 100 one-hundred times
    (for* ([i (range ROWS)]
    	   [j (range i)])
    	(query-exec conn "insert into mapping(k, v) values (?, ?)" i j)))

; equivalent to "letter_to[x] <- letter_from[x] WHERE mapping[x]"
(define model-simple
	(list (rule (predicate 'letter_to (list 'x))
		        (predicate 'letter_from (list 'x))
		        (list (predicate 'mapping (list 'x))))))

; equivalent to "outcome[x] <- treatment[x] WHERE mapping1[x], mapping2[x]"
(define model-multiple
	(list (rule (predicate 'letter_to (list 'x))
		        (predicate 'letter_from (list 'x))
		        (list (predicate 'mapping1 (list 'x))
		              (predicate 'mapping2 (list 'x))))))

; equivalent to "outcome[x] <- treatment[x] WHERE mapping[x]"
(define model-many
	(list (rule (predicate 'outcome (list 'x))
		        (predicate 'treatment (list 'x))
		        (list (predicate 'mapping (list 'x))))))

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
