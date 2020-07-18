#lang racket/base
(require racket/list
         racket/contract)

(struct rule (head body where) #:transparent)
(struct c-query (outcome treatment) #:transparent)
(struct inputs (rules queries) #:transparent)
(struct predicate (name vars) #:transparent)
(provide (contract-out 
          [struct inputs ((rules (listof rule?)) (queries (listof c-query?)))]
          [struct c-query ((outcome predicate?) (treatment predicate?))]
          [struct rule 
               ((head predicate?)
                (body predicate?)
                (where (listof predicate?)))]
          [struct predicate ((name symbol?) (vars (listof symbol?)))]))

(define (handle-inputs m)
    (let* ([datum (syntax->datum m)]
           [xs (cleanup datum)]
           [rs (filter rule? xs)]
           [qs (filter c-query? xs)]  
           [result (inputs rs qs)])
        result))
(provide handle-inputs)

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
