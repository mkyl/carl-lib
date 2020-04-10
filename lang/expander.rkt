#lang racket

(struct rule (head body) #:transparent)
(provide (struct-out rule)) 

(define (handle-model m)
    (let* ([datum (syntax->datum m)]
           [rules (handle-rule datum)])
        rules))
(provide handle-model)

(define (handle-rule r)
    (let* ([result 
        (cond
        [(and (list? r) (eq? 'rule (first r))) (
            let* ([r (rest r)]
                [head (handle-rule (first r))]
                [body (handle-rule (first (rest (rest r))))])
                (rule head body))]
        [(and (list? r) (eq? 'table (first r))) r]
        [(and (list? r) (eq? 'variable (first r))) r]
        [(list? r) (map handle-rule r)]
        [else '()])]
        [result (flatten result)])
      result))
