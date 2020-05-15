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
               [r1 (backdoor-criterion g 'a 'b z)]
               [r2 (backdoor-criterion complex-g 'x 'y (list 'z1 'z2 'z3))]
               [r3 (backdoor-criterion complex-g 'x 'y (list 'z1 'z3))]
               [r4 (backdoor-criterion complex-g 'x 'y (list 'w1 'z3))]
               [r5 (backdoor-criterion complex-g 'x 'y (list 'w2 'z3))])
            (check-true r1)
            (check-true r2)
            (check-true r3)
            (check-true r4)
            (check-true r5)))
     (test-case
        "backdoor-criterion rejects insufficient adjustment"
        (let* ([g simple-g]
               [z empty]
               [r1 (backdoor-criterion g 'a 'b z)]
               [r2 (backdoor-criterion complex-g 'x 'y (list 'z3))]
               [r3 (backdoor-criterion complex-g 'x 'y (list 'w3))]
               [r4 (backdoor-criterion complex-g 'x 'y empty)])
            (check-false r1)
            (check-false r2)
            (check-false r3)
            (check-false r4)))))

(define simple-g (unweighted-graph/directed (list (list 'a 'b) (list 'c 'a) (list 'c 'b))))

; graph from Figure 4, "An Introduction to Causal Inference", Judea Pearl.
; Int J Biostat. 2010 Jan 6; 6(2): 7.
(define complex-g (unweighted-graph/directed (list
    (list 'x 'w3)
    (list 'w3 'y)
    (list 'w1 'x)
    (list 'z3 'x)
    (list 'z3 'y)
    (list 'w2 'y)
    (list 'z1 'w1)
    (list 'z1 'z3)
    (list 'z2 'z3)
    (list 'z2 'w2))))

(provide detect-tests)
