#lang racket/base

(require racket/list
    racket/string
    racket/set
    racket/vector
	db
    graph
    datalog
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

(struct table (name vars))
(struct node (unit name value))
(struct check (tab1 col1 tab2 col2))

(struct atom (unit attr value) #:transparent)
(provide atom)

(define (create-query dbc r)
    (let* ([h (rule-head r)]
           [b (rule-body r)]
           [w (rule-where r)]
           [all (cons h (cons b w))]
           [as (list (string-append (symbol->string (predicate-name h)) ".*")
                     (string-append (symbol->string (predicate-name b)) ".*"))]
           [ts (flatten (map (compose1 symbol->string predicate-name) all))]
           [cs (build-conds dbc all)]
           [attrs (string-join as ", ")]
           [tables (string-join ts ", ")]
           [conds (string-join cs " AND ")])
        (string-append "SELECT DISTINCT " attrs " FROM " tables " WHERE " conds)))

(define (build-conds dbc preds)
    (let* ([g (datalog-graph preds)]
           [l (symbol-lookup dbc preds)]
           [es (get-edges g)]
           [cs (map (lambda (e) (edge->check l g e)) es)]
           [conds (map check->string cs)])
        conds))

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

; create map of (predicate, symbol) : db column name 
(define (symbol-lookup dbc preds)
    (let* ([tables (list-tables dbc)]
           [cols (map (lambda (t) (read-cols dbc t)) tables)]
           [assoc (map cons tables cols)]
           [tab->cols (make-immutable-hash assoc)])
        (make-immutable-hash 
            (for*/list ([p preds]
                        [vs (map cons (predicate-vars p)
                                      (hash-ref tab->cols
                                        (symbol->string (predicate-name p))))])
                (cons (cons (predicate-name p) (car vs)) (cdr vs))))))

(define (edge->check lookup g edge)
    (let* ([t1 (car edge)]
           [t2 (second edge)]
           [s (edge-weight g t1 t2)])
        (check
            t1
            (hash-ref lookup (cons t1 s))
            t2
            (hash-ref lookup (cons t2 s)))))

(define (check->string c)
    (string-append 
        (symbol->string (check-tab1 c)) "." (check-col1 c)
        " = " 
        (symbol->string (check-tab2 c)) "." (check-col2 c)))

(define (query->edges dbc q pred1 pred2)
    (let* ([qrs (query-rows dbc q)]
           [attr1 (predicate-name pred1)]
           [attr2 (predicate-name pred2)]
           [key1-size (length (predicate-vars pred1))]
           [edges (map (lambda (r) (construct-edge r attr1 attr2 key1-size)) qrs)])
        edges))

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

(define (read-cols dbc t-name)
    ; TODO why does this not work? `(query-rows dbc "PRAGMA table_info(?)" table)`
    ; TODO better approach: https://en.wikipedia.org/wiki/Information_schema
    (let* ([rs (query-rows dbc (string-append "PRAGMA table_info(" t-name ")"))]
           [ks (filter isprimary rs)]
           [vars (map get-name ks)])
        vars))

; is this column a primary key? non-zero is pk
(define (isprimary c)
  (not (eq? (vector-ref c PK) 0)))

(define (get-name v)
  (vector-ref v NAME))
