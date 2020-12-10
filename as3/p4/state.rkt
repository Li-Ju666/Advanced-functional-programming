#lang racket
(provide (all-defined-out))


(define (return x)
    (lambda (s) (list x s)))

(define (get s) (list s s))

(define (put x)
    (lambda (s) (list '() x)))

(define (runState state) state)

(define (evalState act)
    (lambda (s) (first (act s))))

(define (>>= act1 fact2)
    (lambda (s)
        (let ([intermed (act1 s)])
            ((fact2 (first intermed)) (second intermed)))))

(define-syntax do
  (syntax-rules (<-)
    [(do exp) exp]
    [(do value <- exp rest ...)
       (>>= exp (lambda (value) (do rest ...)))]
    [(do exp rest ...)
       (>>= exp (lambda (x) (do rest ...)))]))

;(define t '("this" ("relabel" () ()) ("tree" ("binary" () ()) ())))
;(define (relabel x)
;  (if (null? x) (return x)
;      (do l1 <- (relabel (first (rest x)))
;        n1 <- get
;        (put (+ n1 1))
;        r1 <- (relabel (first (rest (rest x))))
;        (return (list n1 l1 r1)))))

;(equal?
; ((evalState (relabel t) ) 1)
; '(2 (1 () ()) (4 (3 () ()) ())))