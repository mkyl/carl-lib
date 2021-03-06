#lang racket/base

(require racket/list
    racket/string
    racket/set
    racket/vector
	db
    graph
	carl-lib/lang)

(define (ground rules dbc)
    (let* ([qs (map (lambda (r) (create-query dbc r)) rules)]
           [heads (map rule-head rules)]
           [bodies (map rule-body rules)]
           [edges (apply append (map (lambda (q h b) (query->edges dbc q h b))
                                      qs heads bodies))]
           [g (directed-graph edges)]) 
        g))

(provide ground)
(provide get-missing)

(struct table (name vars))
(struct node (unit name value))
(struct check (tab1 col1 tab2 col2))

(struct atom (unit attr value) #:transparent)
(provide atom)

; create an SQL query that retrieves edges matching a causal rule
(define (create-query dbc r)
    (let* ([h (rule-head r)]
           [b (rule-body r)]
           [w (rule-where r)]
           [all (flatten (list h b w))]
           [h-name (predicate-name (rule-head r))]
           [b-name (predicate-name (rule-body r))]
           [tables (map string->symbol (list-tables dbc))]
           [all-obs (filter (λ(t) (member (predicate-name t) tables)) all)]
           ; if one of the two tables is unobserved, use the PKs from the observed
           ; table and `null` for the values
           [t1 (if (member h-name tables)
                   (list (string-append (symbol->string h-name) ".*"))
                   (append (map (compose1 (λ(s) (string-append (symbol->string b-name) "." s))
                                      symbol->string) (read-cols dbc b-name)) (list "null")))]
           [t2 (if (member b-name tables)
                   (list (string-append (symbol->string b-name) ".*"))
                   (append (map (compose1 (λ(s) (string-append (symbol->string h-name) "." s))
                                      symbol->string) (read-cols dbc h-name)) (list "null")))]
           [as (append t1 t2)]
           [ts (flatten (map (compose1 symbol->string predicate-name) all-obs))]
           [cs (build-conds dbc all)]
           [attrs (string-join as ", ")]
           [tables (string-join ts ", ")]
           [conds (string-join cs " AND ")])
        (if (empty? cs)
            (string-append "SELECT DISTINCT " attrs " FROM " tables)
            (string-append "SELECT DISTINCT " attrs " FROM " tables " WHERE " conds))))

; build the conditions for the WHERE clause
(define (build-conds dbc preds)
    (let* ([g (datalog-graph preds)]
           [l (symbol-lookup dbc preds)]
           [es (get-edges g)]
           [cs (filter-map (lambda (e) (edge->check l g e)) es)]
           [conds (map check->string cs)])
        conds))

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

(define (get-missing dbc preds)
    (let* ([h (map rule-head preds)]
           [b (map rule-body preds)]
           [w (map rule-where preds)]
           [all (remove-duplicates (flatten (append h b w)))]
           [tables (map string->symbol (list-tables dbc))]
           [missing (filter (λ(x) (not (member (predicate-name x) tables))) all)])
        missing))

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

; convert the WHERE condition struct into a string like "T1.x = T2.y"
(define (check->string c)
    (string-append
        (symbol->string (check-tab1 c)) "." (symbol->string (check-col1 c))
        " = " 
        (symbol->string (check-tab2 c)) "." (symbol->string (check-col2 c))))

; given an sql query, return the edges in the ground graph that
; the query corresponds to
(define (query->edges dbc q pred1 pred2)
    (let* ([qrs (query-rows dbc q)]
           [attr1 (predicate-name pred1)]
           [attr2 (predicate-name pred2)]
           [key1-size (length (predicate-vars pred1))]
           [edges (map (lambda (r) (construct-edge r attr1 attr2 key1-size)) qrs)])
        edges))

; construct an edge in the grounded graph given a query result row
(define (construct-edge row attr1 attr2 key1-size)
    (let*-values ([(to from) (vector-split-at row (add1 key1-size))]
                  [(to-key) (vector-drop-right to 1)]
                  [(to-val) (vector-take-right to 1)]
                  [(from-key) (vector-drop-right from 1)]
                  [(from-val) (vector-take-right from 1)])
       (list (atom (vector->list to-key) attr1 (vector-ref to-val 0))
             (atom (vector->list from-key) attr2 (vector-ref from-val 0)))))
    

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

; return a list of the name of the primary keys of a table
(define (read-cols dbc t-name)
    ; TODO why does this not work? `(query-rows dbc "PRAGMA table_info(?)" table)`
    ; TODO better approach: https://en.wikipedia.org/wiki/Information_schema
    (let* ([rs (query-rows dbc (string-append "PRAGMA table_info("
                                              (symbol->string t-name) ")"))]
           [ks (filter isprimary rs)]
           [vars (map (compose1 string->symbol get-name) ks)])
        vars))

; is this column a primary key? non-zero is pk
(define (isprimary c)
  (not (eq? (vector-ref c PK) 0)))

; get the name of a column
(define (get-name v)
  (vector-ref v NAME))
