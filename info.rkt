#lang info
(define collection "carl-lib")
(define deps '("base" "brag-lib" "db" "graph" "math-lib" "rackunit-lib"
	"scribble-lib" "csv-writing" "graphviz"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "math-doc"))
(define scribblings '(("scribblings/carl-lib.scrbl" ())))
(define pkg-desc "Causal Relational Learning library")
(define version "0.1.0")
(define pkg-authors '(Moe Kayali))
