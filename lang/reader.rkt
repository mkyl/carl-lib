#lang racket
(require "parser.rkt")

(define (load-rules input-port)
    (let* ([lex (apply-port-proc carl-lexer input-port)]
           [ast (parse lex)])
        ast))
(provide load-rules)

(require brag/support)
(define carl-lexer
    (lexer
        ["\n" (token 'NEWLINE lexeme)]
        [whitespace (token lexeme #:skip? #t)]
        ["[" (token 'LEFT-BRACKET lexeme)]
        ["]" (token 'RIGHT-BRACKET lexeme)]
        ["<-" (token 'CAUSES lexeme)]
        [(repetition 1 +inf.0 (union alphabetic "_")) (token 'ID lexeme)]))
