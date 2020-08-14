#lang racket/base

(require racket/contract
		 scribble/srcdoc
		 (for-doc racket/base scribble/manual)
		 graph
		 db
		 carl-lib/lang
		 "ground/load.rkt"

         racket/list
         racket/set
         racket/string)

(struct Z-query-parts (tables where-expr z) #:transparent)
(struct check (tab1 col1 tab2 col2))

; takes a path, a lifted causal graph, and table info dict 
; returns (tables, WHERE clause)
(define (path-conditions P G db)
    (define t-names
        (for/list ([p P]) (predicate-name p)))
    (define edge-names
        (apply append (for/list ([p1 P] [p2 (cdr P)])
            (map predicate-name (edge-weight G p1 p2)))))
    (Z-query-parts
        ;tables
        (list->set (append t-names edge-names))
        ;where expressions
        (for/list ([p1 P] [p2 (cdr P)])
            (where-expr G db p1 p2))
        ; z predicate
        (car P)))

; given two predicates
(define (where-expr G db p1 p2)
    (define C
        (edge-weight G p1 p2))
    (define preds
        (cons p1 (cons p2 C)))
    (build-conds db preds))

(define (build-covar-query parts y db)
    (define T
        (for/list ([t (Z-query-parts-tables parts)])
            (symbol->string t)))
    (define W
        (apply append (Z-query-parts-where-expr parts)))
    (define z
        (predicate-name (Z-query-parts-z parts)))
    (define y-keys
        (for/list ([k (read-cols db (predicate-name y))])
            (string-append (symbol->string (predicate-name y)) "."
                           (symbol->string k))))
    (define y-val
        (tab-val db (predicate-name y)))
    (string-append "SELECT " (string-join y-keys ",") ", AVG(" (tab-val db z) ")"
                   " FROM " (string-join T ", ")
                   " WHERE " (string-join W " AND ")
                   " GROUP BY " (string-join y-keys ",")))

(define (build-unit-table db t y C ZQ)
    (define y-val
        (tab-val db (predicate-name y)))
    (define y-keys
        (for/list ([k (read-cols db (predicate-name y))])
            (string-append (symbol->string (predicate-name y)) "."
                           (symbol->string k))))
    (define y-keys-clean
        (map (λ(k) (string-replace k "." "_")) y-keys))
    ; without "." to avoid syntax errors
    (define t-val
        (tab-val db (predicate-name t)))
    (define CTE-names
        (for/list ([_ ZQ]) (gensym "Z_")))
    (define CTEs
        (for/list ([z ZQ] [name CTE-names])
            (string-append (symbol->string name) 
                "(" (string-join y-keys-clean ",") ", V) AS (" z ")")))
    (define tables
        (map symbol->string 
            (append (list (predicate-name t) (predicate-name y))
                    (map predicate-name C)
                    CTE-names)))
    (define Z-conds
        (for*/list ([name (map symbol->string CTE-names)]
                    [k y-keys])
            ; TODO de-duplicate with `y-keys-clean`
            (string-append name "." (string-replace k "." "_") "=" k)))
    (define C-conds
        (build-conds db (cons t (cons y C))))
    (define conds
        (append C-conds Z-conds))
    (define CTE-vals
            (for/list ([name CTE-names])
                (string-append (symbol->string name) ".V")))
    (define columns
        (for/list ([c (append (list t-val y-val) CTE-vals)])
            (string-append "AVG(" c ")")))

    (string-append (string-join CTEs ", " #:before-first "WITH ")
        " SELECT " (string-join columns ", ")
        " FROM " (string-join tables ", ")
        " WHERE " (string-join conds " AND ")
        " GROUP BY " (string-join y-keys ", ")))

(define (ground-direct db t y C Z G)
    (define P
        (for/list ([z Z])
            (fewest-vertices-path G z y)))
    ; collect parts to build queries
    (define ZQ-parts
        (for/list ([p P])
            (path-conditions p G db)))
    ; queries for each covariate
    (define ZQ
        (for/list ([qp ZQ-parts])
            (build-covar-query qp y db)))
    ; query that creates the unit table
    (define Q
        (build-unit-table db t y C ZQ))
    ; result
    (query-rows db Q))

(define (tab-val db t-name)
    (let* ([rs (query-rows db (string-append "PRAGMA table_info("
                                              (symbol->string t-name) ")"))]
           [ks (filter (λ(r) (not (isprimary r))) rs)]
           [vars (map get-name ks)])
        (string-append (symbol->string t-name) "." (car vars))))

; copied
; build the conditions for the WHERE clause
(define (build-conds dbc preds)
    (let* ([g (datalog-graph preds)]
           [l (symbol-lookup dbc preds)]
           [es (get-edges g)]
           [cs (filter-map (lambda (e) (edge->check l g e)) es)]
           [conds (map check->string cs)])
        conds))

; copied
; create a graph of tables and logical variables joining them in the query
(define (datalog-graph preds)
    (let* (
        [cs (combinations preds 2)]
        [cs-vars (map (lambda (t) (map predicate-vars t)) cs)]
        [cs-preds (map (lambda (t) (map predicate-name t)) cs)]
        [is (map (lambda (ss) (apply set-intersect ss)) cs-vars)]
        [edges (apply append (map (lambda (p vs)
                            (map (lambda (v) (list v (car p) (second p))) vs))
                                cs-preds is))])
      (weighted-graph/directed edges)))

; copied
; create map of (predicate, symbol) : db column name 
(define (symbol-lookup dbc preds)
    (let* ([tables (map string->symbol (list-tables dbc))]
           [cols (map (lambda (t) (read-cols dbc t)) tables)]
           [assoc (map cons tables cols)]
           [tab->cols (make-immutable-hash assoc)]
           [pred-names (remove-duplicates (map predicate-name preds))]
           [missing (set-subtract pred-names tables)]
           [_ (if (not (set-empty? missing))
                  (displayln (list "WARNING: assuming predicates are unobserved:"
                                   missing))
                  '())]
           [preds-obs (filter (λ(x) (member (predicate-name x) tables)) preds)])
        (make-immutable-hash 
            (for*/list ([p preds-obs]
                        [vs (map cons (predicate-vars p)
                                      (hash-ref tab->cols (predicate-name p)))])
                (cons (cons (predicate-name p) (car vs)) (cdr vs))))))

; copied
; convert the WHERE condition struct into a string like "T1.x = T2.y"
(define (check->string c)
    (string-append
        (symbol->string (check-tab1 c)) "." (symbol->string (check-col1 c))
        " = " 
        (symbol->string (check-tab2 c)) "." (symbol->string (check-col2 c))))

; copied
; convert an edge in the graph of tables into a condition
(define (edge->check lookup g edge)
    (let* ([t1 (car edge)]
           [t2 (second edge)]
           [s (edge-weight g t1 t2)]
           [k1 (cons t1 s)]
           [k2 (cons t2 s)])
        (if (and (hash-has-key? lookup k1) (hash-has-key? lookup k2))
            (check
                t1
                (hash-ref lookup k1)
                t2
                (hash-ref lookup k2))
            #f)))

; copied
; PRAGMA table_info COLUMNS
(define CID
  0)
(define NAME
  1)
(define TYPE
  2)
(define NOTNULL
  3)
(define DFLT_VALUE
  4)
(define PK
  5)

; copied
; return a list of the name of the primary keys of a table
(define (read-cols dbc t-name)
    ; TODO why does this not work? `(query-rows dbc "PRAGMA table_info(?)" table)`
    ; TODO better approach: https://en.wikipedia.org/wiki/Information_schema
    (let* ([rs (query-rows dbc (string-append "PRAGMA table_info("
                                              (symbol->string t-name) ")"))]
           [ks (filter isprimary rs)]
           [vars (map (compose1 string->symbol get-name) ks)])
        vars))

; copied
; is this column a primary key? non-zero is pk
(define (isprimary c)
  (not (eq? (vector-ref c PK) 0)))

; copied
; get the name of a column
(define (get-name v)
  (vector-ref v NAME))


(provide (proc-doc/names ground ((listof rule?) connection?
	. -> . (and/c graph? unweighted-graph?)) (model conn) 
	("Combine CaRL rules with a database instance to build a ground causal model.")))

(provide (contract-out
    [struct atom ((unit list?) (attr symbol?) (value any/c))]))

(provide get-missing
    ground-direct)
