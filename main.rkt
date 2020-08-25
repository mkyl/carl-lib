#lang racket/base

(module+ main
    (require carl-lib/lang))

(module+ test
    (require rackunit/text-ui
        rackunit
        "test/lang.rkt"
        "test/ground.rkt"
        "test/detect.rkt"
        "test/integration.rkt"
        "test/embed.rkt"))

(module+ test
    ;; Any code in this `test` submodule runs when this file is run using DrRacket
    ;; or with `raco test`. The code here does not run when this file is
    ;; required by another module.
    (define all-tests (test-suite
        "All tests"
        lang-tests
        ground-tests
        detect-tests
        integration-tests
        embed-tests))
    (run-tests all-tests))

(require carl-lib/lang
         carl-lib/ground
         carl-lib/embed
         carl-lib/detect
         carl-lib/unit-table
         carl-lib/estimate
         racket/list
         graph
         graphviz
         pict)
(provide compute)
(define (compute f db fast) (let* 
  ([start (current-inexact-milliseconds)]
   [m (create-inputs f)]
   [T (c-query-treatment (first (inputs-queries m)))]
   [Y (c-query-outcome (first (inputs-queries m)))]
   [C (c-query-where (first (inputs-queries m)))]
   [G (causal-path-graph (inputs-rules m) T Y)]
   [missing (get-missing db (inputs-rules m))]
   [Z (detect (inputs-rules m) missing T Y)]
   [_ (display "adjusting for: ")]
   [_ (displayln Z)]
   ; TODO enable support for more than 1 query
   [table (ground-direct db T Y C Z G)]
   [table (map vector->list table)]
   [stop (current-inexact-milliseconds)]
   [_ (display "time elapsed: ")]
   [_ (display (- stop start))]
   [_ (displayln " milliseconds ")]
   [_ (for ([v (get-vertices G)]) (rename-vertex! G v (predicate-name v)))]
   [G2 (unweighted-graph/undirected (get-edges G))]
   [colors (cons (cons (predicate-name T) 0.1) (cons (cons (predicate-name Y) 0.5)
             (map (λ(z) (cons (predicate-name z) 0.95)) Z)))]
   [C (make-hash colors)]
   [viz (graphviz G2 #:colors C)]
   [ate (estimate table fast)])
  (flatten (list "Lifted Causal Graph:" 
    (pict->bitmap (dot->pict viz)) 
    "[Legend: Treatment=Orange, Outcome=Green, Confounders=Blue]" "." "."
     "Average Treatment Effect (ATE) estimate:"
     ate))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline
           db
           carl-lib/lang
           carl-lib/ground
           carl-lib/embed
           carl-lib/detect
           carl-lib/unit-table
           carl-lib/estimate)
  (define (run filename db-location)
    (let* ([f (open-input-file filename)]
           [db (sqlite3-connect #:database db-location)])
          (compute f db)))
  (command-line
    #:program "CaRL"
    #:args (filename db)
    (print (run filename db))))
