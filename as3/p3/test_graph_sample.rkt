#lang racket
(module test-helper racket
  (provide (all-defined-out))
  (define (fail? f)
    (eq? 'fail f))

  (define score 0)
  (define count 0)
  (define (scored _) (set! score (add1 score)))
  (define test
    (lambda ()
      (set! count (add1 count))
      (printf "Test ~s~n" count)))

  (define-syntax-rule (should-fail foo)
                      (with-handlers ([exn:fail:contract? (lambda (exn) (scored 1) 'ok)]
                                      [fail? (lambda (exn) 'fail)])
                                     (test)
                                     foo
                                     'fail))

  (define-syntax-rule (should-succeed foo)
                      (with-handlers ([exn? (lambda (exn) 'fail)])
                                     (test)
                                     foo
                                     (scored 1)
                                     'ok))

  (define-syntax-rule (returns-true foo)
    (with-handlers ([exn:fail:contract? (lambda (exn) 'fail)]
                    [fail? (lambda (exn) 'fail)])
                    (test)
                    (if foo (begin (scored 1) 'ok) ('fail))))

  )

(module test-good-graph racket
  (provide score count)

  (require "graph1.rkt")
  (require (submod ".." test-helper))

  (define foo (grader-new))

  "Able to add nodes:"
  (should-succeed (add_vertex foo 1))
  )

(module test-bad-graph racket
  (provide score count)
  (require "graph2.rkt")
  (require (submod ".." test-helper))

  (define foo (grader-new))

  "Able to add nodes:"
  (should-succeed (add_vertex foo 1))
  )

(module task-header racket
  (displayln "Task 1 - Contract")
  (printf "================~n~n")
  )

(require 'task-header)
(require 'test-good-graph)
(require 'test-bad-graph)

(printf "~nSuccessful tests: ~s/~s~n" score count)
(define total-score 3.)
(printf (format "~%Grade: ~a/~a~%~%"
        (* total-score (/ score count))
        total-score))
