#lang racket/base
(require racket/list
         racket/contract
         syntax/parse)

(require (for-syntax syntax/parse))

(struct rule (head body where) #:prefab)
(struct c-query (outcome treatment where) #:prefab)
(struct inputs (rules queries) #:transparent)
(struct predicate (name vars) #:prefab)
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
    (let* ([s (parse-carl m)]
           [all (syntax->datum s)]
           [rules (filter rule? all)]
           [queries (filter c-query? all)]
           [result (inputs rules queries)])
        result))

(provide handle-inputs)

(define (parse-carl s)
    (syntax-parse s
        [((~literal model) (~alt "\n" x) ...)
            (with-syntax ([y (map parse-carl (syntax-e #'(x ...)))]) #'y)]
        [((~datum line) x) (with-syntax ([x (parse-carl (syntax-e #'x))]) #'x)]
        [((~datum query) r "?")
            (with-syntax ([r (rule->c-query (syntax->datum (parse-carl #'r)))])
                #'r)]
        [((~datum rule1) p1 "<-" p2 
          (~seq (~optional "where") (~optional p3 #:defaults ([p3 #'(predicate-list )]))))
         (with-syntax ([p1 (parse-carl #'p1)]
                        [p2 (parse-carl #'p2)]
                        [p3 (parse-carl #'p3)])
            #'#s(rule p1 p2 p3))]
        [((~datum predicate) name "[" vars "]") 
                  (with-syntax ([name (parse-carl #'name)]
                                [vars (parse-carl #'vars)]) 
                    #'#s(predicate name vars))]
        [((~datum symbol) x) (with-syntax ([x (string->symbol (syntax->datum #'x))])
                                #'x)]
        [((~or (~datum symbol-list)
               (~datum predicate-list)) (~alt "," x) ...)
            (with-syntax ([x (map parse-carl (syntax-e #'(x ...)))]) #'x)]))

(define (rule->c-query r)
    (c-query (rule-head r) (rule-body r) (rule-where r)))

