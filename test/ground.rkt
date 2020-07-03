#lang racket/base

(require rackunit
    db
    graph
    carl-lib/lang
    carl-lib/ground)

(provide ground-tests)

(define ground-tests
	(test-suite
		"Grounding tests"
		'()))
