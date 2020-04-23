#lang racket/base
(require racket/list)

(struct rule (head body) #:transparent)
(struct ques (outcome treatment) #:transparent)
(struct model (rules queries) #:transparent)
(struct table (name) #:transparent)
(provide (struct-out rule)
         (struct-out ques)
         (struct-out model)
         (struct-out table))

(define (handle-model m)
    (let* ([datum (syntax->datum m)]
           [xs (cleanup datum)]
           [rs (filter rule? xs)]
           [qs (filter ques? xs)]
           [result (model rs qs)])
        result))
(provide handle-model)

(define (cleanup m)
    (cond [(and (list? m) (equal? (first m) 'table)) (table (first (rest m)))]
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
        (ques outcome treatment)))
