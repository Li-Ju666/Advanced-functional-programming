#lang racket

(require "state.rkt")

(define t '("this" ("relabel" () ()) ("tree" ("binary" () ()) ())))

(define (relabel x) =                 ; Use x here to demonstrate hygiene
  (if (null? x) (return x)
    (do l1 <- (relabel (first (rest x)))
        n1 <- get
	(put (+ n1 1))
	r1 <- (relabel (first (rest (rest x))))
	(return (list n1 l1 r1)))))

(equal?
  ((evalState (relabel t) )1)
  '(2 (1 () ()) (4 (3 () ()) ())))
