#lang racket

(require racket/engine)
(require racket/include)
; (include "kcolor.rkt")
(require "kcolor.rkt")


(define max-score 4.0)
(define timeout-seconds 60)

(define (check-solution graph num-colors is-possible solution)
  (begin
    (fprintf (current-output-port) "WARNING: THIS WILL ALWAYS RETURN SUCCESS~n")
    #t))

; 
; Inputs 
; 

(define inputs
  `( 
   (((1 (2 3)) (2 (1 3)) (3 (1 2))) 3 #t) 
   (((1 (2 3)) (2 (1 3)) (3 (1 2))) 1 #f)))

(define (grade-helper tests correct total)
  (cond [(empty? tests) (list correct total)]
        [#t (let* ([test (car tests)]
                   [graph       (list-ref test 0)]
                   [num-colors  (list-ref test 1)]
                   [is-possible (list-ref test 2)]
                   [e       (engine (lambda (_) (kcolor graph num-colors)))]
                   [eres    (engine-run (* 1000 timeout-seconds) e)])
              (if (equal? eres #t)
                  (if (check-solution graph num-colors is-possible (engine-result e))
                  (begin
                    (fprintf (current-output-port) "Test ~a: [SUCCESS]~n" (+ total 1))
                    (grade-helper (cdr tests) (+ correct 1) (+ total 1)))
                  (begin
                    (fprintf (current-output-port) "Test ~a: [FAILURE]~n" (+ total 1))
                    (grade-helper (cdr tests) correct (+ total 1)))
                   )
                  (begin
                    (fprintf (current-output-port) "Test ~a: Timeout after ~a second~n" (+ total 1 ) timeout-seconds)
                    (grade-helper (cdr tests) correct (+ total 1)))))]))

(define (grade tests) (grade-helper inputs 0 0))

(module* main #f
  (define result (grade inputs))
  (define correct (list-ref result 0))
  (define total (list-ref result 1))
  (define final_grade (/ (* correct max-score) total))
  (fprintf (current-output-port) "Total tests: ~a/~a~n" correct total)
  (fprintf (current-output-port) "\nGrade: ~a / ~a~n" final_grade max-score)
  )
