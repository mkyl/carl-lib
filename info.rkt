#lang info
(define collection "carl-lib")
(define deps '("base" "brag-lib" "db" "graph"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/carl-lib.scrbl" ())))
(define pkg-desc "Causal Relational Learning library")
(define version "0.0.1")
(define pkg-authors '(Moe Kayali))
