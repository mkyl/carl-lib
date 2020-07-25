#lang racket/base
(require racket/list
         racket/contract
         syntax/parse)

(require (for-syntax syntax/parse))

(struct rule (head body where) #:transparent)
(struct c-query (outcome treatment where) #:transparent)
(struct inputs (rules queries) #:transparent)
(struct predicate (name vars) #:transparent)
(provide (contract-out 
          [struct inputs ((rules (listof rule?)) (queries (listof c-query?)))]
          [struct c-query ((outcome predicate?)
                           (treatment predicate?)
                           (where (listof predicate?)))]
          [struct rule 
               ((head predicate?)
                (body predicate?)
                (where (listof predicate?)))]
          [struct predicate ((name symbol?) (vars (listof symbol?)))]))

(define (handle-inputs m)
    (let* ([_ (displayln (syntax->datum (parse-carl m)))]
           [datum (syntax->datum m)]
           [xs (cleanup datum)]
           [rs (filter rule? xs)]
           [qs (filter c-query? xs)]  
           [result (inputs rs qs)])
        result))
(provide handle-inputs)

(define (parse-carl s)
    (syntax-parse s
        [((~literal model) ~rest x)
            (with-syntax ([x (map parse-carl (syntax-e #'x))]) #'x)]
        [((~datum line) x) (with-syntax ([x (parse-carl (syntax-e #'x))]) #'x)]
        [((~datum query) r "?") (with-syntax ([r (parse-carl #'r)])
                                    'query_todo)]
        [((~datum rule) p1 "<-" p2) (with-syntax ([p1 (parse-carl #'p1)]
                                                  [p2 (parse-carl #'p2)])
                                        #'(rule p1 p2 void))]
        [((~datum predicate) name "[" vars "]") 
                  (with-syntax ([name (parse-carl #'name)]
                                [vars (parse-carl #'vars)]) 
                      #'(predicate name vars))]
        [((~datum symbol) x) #'x]
        ["\n" #'void]
        [((~or (~datum symbol-list)
               (~datum predicate-list)) ~rest x)
            (with-syntax ([x (map parse-carl (syntax-e #'x))]) #'x)]))

(define (rule->c-query r)
    (let ([r (c-query (rule-head r) (rule-body r) (rule-where r))])
        #'r))

(define (cleanup m)
    (cond [(and (list? m) (equal? (first m) 'table)) (predicate (first (rest m)))]
          [(and (list? m) (equal? (first m) 'rule)) (handle-rule m)]
          [(and (list? m) (equal? (first m) 'query)) (handle-query m)]
          [(list? m) (flatten (filter-map cleanup m))]
          [else #f]))

(define (handle-rule r)
    (let* ([cleaned (cleanup (rest r))]
           [head (first cleaned)]
           [body (first (rest cleaned))])
        (rule head body)))

(define (handle-query q)
    (let* ([cleaned (cleanup (rest q))]
           [outcome (first cleaned)]
           [treatment (first (rest cleaned))])
        (c-query outcome treatment)))
