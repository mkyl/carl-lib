#lang racket/base

(module+ main
    (require carl-lib/lang))

(module+ test
    (require rackunit/text-ui
        "test/lang.rkt"))

(module+ test
    ;; Any code in this `test` submodule runs when this file is run using DrRacket
    ;; or with `raco test`. The code here does not run when this file is
    ;; required by another module.

    (run-tests lang-tests))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline
           carl-lib/lang
           carl-lib/ground
           carl-lib/embed
           carl-lib/detect
           carl-lib/unit-table
           carl-lib/estimate)
  (define (run filename db) (let* 
    ([f (open-input-file filename)]
     [model (create-model f)]
     [gcm (ground model db)]
     [aug-gcm (embed gcm)]
     [Z (detect aug-gcm)]
     [table (construct aug-gcm Z)]
     [ate (estimate table)]) 
    ate))
  (command-line
    #:program "CaRL"
    #:args (filename db)
    (print (run filename db))))
