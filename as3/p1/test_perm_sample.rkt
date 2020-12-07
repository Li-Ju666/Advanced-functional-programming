#lang racket

(require racket/engine)
(require racket/include)
(require "perm.rkt")

(define (reverse-couples l)
  (match l [(list a b) (list b a)]
           [(list a) (list a)]
           [(list-rest a b rest) (cons b (cons a (reverse-couples rest)))]))

(define max-score 4.0)
(define timeout-seconds 2)

(define inputs
  `( 
     ((1) (1) #t)
     ((1 2) (1 2) #t)))

(define (grade-helper tests correct total)
  (cond [(empty? tests) (list correct total)]
        [#t (let* ([test (car tests)]
                   [input   (list-ref test 0)]
                   [output  (list-ref test 1)]
                   [is_perm (list-ref test 2)]
                   [e       (engine (lambda (_) (perm input output)))]
                   [eres    (engine-run (* 1000 timeout-seconds) e)])
              (if (equal? eres #t)
                  (if (equal? (engine-result e) is_perm)
                  (begin
                    (fprintf (current-output-port) "Test ~a: SUCCESS~n" (+ total 1))
                    (grade-helper (cdr tests) (+ correct 1) (+ total 1)))
                  (begin
                    (fprintf (current-output-port) "Test ~a: FAILURE~n" (+ total 1))
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
