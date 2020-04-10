#lang racket/base

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

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
