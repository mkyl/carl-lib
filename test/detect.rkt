#lang racket/base

(require rackunit
    racket/list
    graph
    "../detect/backdoor.rkt")

(define detect-tests
    (test-suite
     "Covariate Detection"
     (test-case
        "backdoor-criterion accepts sufficient adjustment"
        (let* ([g simple-g]
               [z (list 'c)]
               [r (backdoor-criterion g 'a 'b z)])
            (check-true r)))
     (test-case
        "backdoor-criterion rejects insufficient adjustment"
        (let* ([g simple-g]
               [z empty]
               [r (backdoor-criterion g 'a 'b z)])
            (check-false r)))))

(define simple-g (unweighted-graph/directed (list (list 'a 'b) (list 'c 'a) (list 'c 'b))))

(provide detect-tests)
